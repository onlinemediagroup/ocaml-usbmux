open Lwt.Infix
module T = ANSITerminal
module B = Yojson.Basic
module U = Yojson.Basic.Util

let byte_swap_16 value =
  ((value land 0xFF) lsl 8) lor ((value lsr 8) land 0xFF)

let time_now () =
  let open Unix in
  let localtime = localtime (time ()) in
  Printf.sprintf "[%02u:%02u:%02u]" localtime.tm_hour localtime.tm_min localtime.tm_sec

let colored_message
    ?(time_color=T.Yellow)
    ?(message_color=T.Blue)
    ?(with_time=true)
    str =
  let just_time = T.sprintf [T.Foreground time_color] "%s " (time_now ()) in
  let just_message = T.sprintf [T.Foreground message_color] "%s" str in
  if with_time then just_time ^ just_message else just_message

let error_with_color msg = colored_message ~time_color:T.White ~message_color:T.Red msg

let log_info_bad ?exn msg = error_with_color msg |> Lwt_log.info ?exn

let log_info_sucess msg =
  colored_message ~time_color:T.White ~message_color:T.Yellow msg
  |> Lwt_log.info

let ( >> ) x y = x >>= fun () -> y

let with_retries ?(wait_between_failure=1.0) ?(max_retries=3) ?exn_handler prog =
  assert (max_retries > 0 && max_retries < 20);
  assert (wait_between_failure > 0.0 && wait_between_failure < 10.0);
  let rec do_start current_count () =
    if current_count = max_retries
    then Lwt_io.printlf "Tried %d times and gave up" current_count
    else begin
      Lwt.catch
        prog
        (match exn_handler with Some f -> f | None -> Unix.(function
             | Unix_error (_, name, _) ->
               log_info_bad (Printf.sprintf "Attempt %d, %s failed" (current_count + 1) name) >>
               Lwt_unix.sleep wait_between_failure >>= do_start (current_count + 1)
             | exn ->
               log_info_bad ~exn (Printf.sprintf "Attempt %d" (current_count + 1)) >>
               Lwt_unix.sleep wait_between_failure >>= do_start (current_count + 1)
           ))
    end
  in
  do_start 0 ()

module Protocol = struct

  type msg_version_t = Binary | Plist

  type conn_code = Success
                 | Device_requested_not_connected
                 | Port_requested_not_available
                 | Malformed_request

  type event = Attached of device_t
             | Detached of device_id
  and device_id = int
  and device_t = { serial_number : string;
                   connection_speed : int;
                   connection_type : string;
                   product_id : int;
                   location_id : int;
                   device_id : int; }

  type msg_t = Result of conn_code
             | Event of event

  exception Unknown_reply of string

  let (header_length, usbmuxd_address) = 16, Unix.ADDR_UNIX "/var/run/usbmuxd"

  let listen_message =
    Plist.(Dict [("MessageType", String "Listen");
                 ("ClientVersionString", String "ocaml-usbmux");
                 ("ProgName", String "ocaml-usbmux")]
           |> make)

  (* Note: PortNumber must be network-endian, so it gets byte swapped here *)
  let connect_message ~device_id ~device_port =
    Plist.((Dict [("MessageType", String "Connect");
                  ("ClientVersionString", String "ocaml-usbmux");
                  ("ProgName", String "ocaml-usbmux");
                  ("DeviceID", Integer device_id);
                  ("PortNumber", Integer (byte_swap_16 device_port))])
           |> make)

  let msg_length msg = String.length msg + header_length

  let listen_msg_len = msg_length listen_message

  let read_header i_chan =
    i_chan |> Lwt_io.atomic begin fun ic ->
      Lwt_io.LE.read_int32 ic >>= fun raw_count ->
      Lwt_io.LE.read_int32 ic >>= fun raw_version ->
      Lwt_io.LE.read_int32 ic >>= fun raw_request ->
      Lwt_io.LE.read_int32 ic >|= fun raw_tag ->
      Int32.(to_int raw_count, to_int raw_version, to_int raw_request, to_int raw_tag)
    end

  (** Highly advised to only change value of version of default values *)
  let write_header ?(version=Plist) ?(request=8) ?(tag=1) ~total_len o_chan =
    o_chan |> Lwt_io.atomic begin fun oc ->
      (List.map Int32.of_int [total_len; if version = Plist then 1 else 0; request; tag])
      |> Lwt_list.iter_s (Lwt_io.LE.write_int32 oc)
    end

  let parse_reply raw_reply =
    let handle = Plist.parse_dict raw_reply in
    match U.member "MessageType" handle |> U.to_string with
    | "Result" -> (match U.member "Number" handle |> U.to_int with
        | 0 -> Result Success
        | 2 -> Result Device_requested_not_connected
        | 3 -> Result Port_requested_not_available
        | 5 -> Result Malformed_request
        | n -> raise (Unknown_reply (Printf.sprintf "Unknown result code: %d" n)))
    | "Attached" ->
      Event (Attached
               {serial_number = U.member "SerialNumber" handle |> U.to_string;
                connection_speed = U.member "ConnectionSpeed" handle |> U.to_int;
                connection_type = U.member "ConnectionType" handle |> U.to_string;
                product_id = U.member "ProductID" handle |> U.to_int;
                location_id = U.member "LocationID" handle |> U.to_int;
                device_id = U.member "DeviceID" handle |> U.to_int ;})
    | "Detached" -> Event (Detached (U.member "DeviceID" handle |> U.to_int))
    | otherwise -> raise (Unknown_reply otherwise)

  let create_listener ?event_cb ~max_retries =
    with_retries ~max_retries begin fun () ->
      Lwt_io.with_connection usbmuxd_address begin fun (mux_ic, mux_oc) ->
        (* Send the header for our listen message *)
        write_header ~total_len:listen_msg_len mux_oc >>
        Lwt_io.write_from_string_exactly mux_oc listen_message 0 (String.length listen_message) >>
        read_header mux_ic >>= fun (msg_len, _, _, _) ->
        let buffer = Bytes.create (msg_len - header_length) in

        let rec start_listening () =
          read_header mux_ic >>= fun (msg_len, _, _, _) ->
          let buffer = Bytes.create (msg_len - header_length) in
          Lwt_io.read_into_exactly mux_ic buffer 0 (msg_len - header_length) >>
          match event_cb with
          | None -> start_listening ()
          | Some g -> g (parse_reply buffer) >>= start_listening
        in
        Lwt_io.read_into_exactly mux_ic buffer 0 (msg_len - header_length) >>
        match event_cb with
        | None -> start_listening ()
        | Some g -> g (parse_reply buffer) >>= start_listening

      end
    end

end

module Relay = struct

  let running_relays : unit Lwt.t list ref = ref []

  let pid_file = "/var/run/gandalf.pid"

  let echo ic oc = Lwt_io.(write_chars oc (read_chars ic))

  let load_mappings file_name =
    Lwt_io.lines_of_file file_name |> Lwt_stream.to_list >|= fun all_names ->
    let prepped = all_names |> List.fold_left begin fun accum line ->
        match (Stringext.trim_left line).[0] with
        (* Ignore comments *)
        | '#' -> accum
        | _ ->
          match Stringext.split line ~on:':' with
          | udid :: port_number :: [] -> (udid, int_of_string port_number) :: accum
          | _ -> assert false
      end []
    in
    let t = Hashtbl.create (List.length prepped) in
    prepped |> List.iter (fun (k, v) -> Hashtbl.add t k v);
    t

  let do_tunnel (port, device_id, udid) =
    let server_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    let _ = Lwt_io.establish_server server_address begin fun (tcp_ic, tcp_oc) ->
        Lwt_io.with_connection Protocol.usbmuxd_address begin fun (mux_ic, mux_oc) ->
          (* Hard coded to assume ssh at the moment *)
          let msg = Protocol.connect_message ~device_id ~device_port:22 in
          let open Protocol in
          write_header ~total_len:(msg_length msg) mux_oc >>
          Lwt_io.write_from_string_exactly mux_oc msg 0 (String.length msg) >>
          (* Read the reply, should be good to start just raw piping *)
          read_header mux_ic >>= fun (msg_len, _, _, _) ->
          let buffer = Bytes.create (msg_len - header_length) in
          Lwt_io.read_into_exactly mux_ic buffer 0 (msg_len - header_length) >>
          match parse_reply buffer with
          | Result Success ->
            (Printf.sprintf "Tunneling. Udid: %s Port: %d \
                             Device_id: %d" udid port device_id)
            |> log_info_sucess >>
            echo tcp_ic mux_oc <&> echo mux_ic tcp_oc
          | Result Device_requested_not_connected ->
            (Printf.sprintf "Tunneling: Device requested was not connected. \
                             Udid: %s Device_id: %d" udid device_id)
            |> log_info_bad
          | Result Port_requested_not_available ->
            (Printf.sprintf "Tunneling. Port requested wasn't available. \
                             Udid: %s Port: %d Device_id: %d" udid port device_id)
            |> log_info_bad
          | _ -> Lwt.return_unit
        end
        |> Lwt.ignore_result
      end
    in
    let this_thread = fst (Lwt.task ()) in
    (* Register the thread *)
    running_relays := this_thread :: !running_relays;
    this_thread

  let create_pid_file () =
    Unix.(
      try
        let open_pid_file = openfile pid_file [O_RDWR; O_CREAT; O_CLOEXEC] 0o666 in
        let current_pid = getpid () |> string_of_int in
        write open_pid_file current_pid 0 (String.length current_pid) |> ignore;
        close open_pid_file
      with Unix_error(EACCES, _, _) ->
        error_with_color (Printf.sprintf "Couldn't open pid file %s, \
                                          make sure you have right permissions"
                            pid_file)
        |> prerr_endline;
        exit 2;
    )

  let reload_mapping () =
    let open_pid_file = open_in pid_file in
    let target_pid = input_line open_pid_file |> int_of_string in
    close_in open_pid_file;
    (* Could throw exception on Unix.ESRCH, aka that process id
       doesn't exist, not handling because that would be really
       weird *)
    try
      Unix.kill target_pid Sys.sigusr1;
      exit 0
    with
      Unix.Unix_error(Unix.EPERM, _, _) ->
      error_with_color
        (Printf.sprintf "Couldn't reload mapping, must have correct permissions")
      |> prerr_endline;
      exit 3

  let () =
    (* Since we spin up the TCP server + echo threads with Lwt.async,
       then any exceptions that they would raise are gonna come this
       way. Since we are making the TCP server + echo threads of type
       cancellable, their exception is the Lwt.Canceled hook, in which
       case we do nothing *)
    Lwt.async_exception_hook := function
      | Lwt.Canceled -> ()
      | e ->
        error_with_color
          (Printf.sprintf "Unhandled async exception: %s" (Printexc.to_string e))
        |> prerr_endline;
        exit 4

  let rec begin_relay ~device_map ~max_retries do_daemonize =
    (* Ask for larger internal buffers for Lwt_io function rather than
       the default of 4096 *)
    Lwt_io.set_default_buffer_size 32768;

    (* Broken SSH pipes shouldn't exit our program *)
    Sys.(signal sigpipe Signal_ignore) |> ignore;

    (* Stop the running threads, call begin_relay again *)
    Sys.(signal sigusr1 (Signal_handle begin fun _ ->
        !running_relays |> List.iter Lwt.cancel;
        (* begin_relay ~device_map ~max_retries:(max_retries - 1) false |> Lwt.ignore_result *)
      end))
    |> ignore;

    with_retries ~max_retries begin fun () ->
      load_mappings device_map >>= fun device_mapping ->
      let devices = Hashtbl.create 12 in
      try%lwt
        (* We do this because usbmuxd itself assigns device IDs and we
           need to begin the listen message, then find out the device IDs
           that usbmuxd has assigned per UDID, hence the timeout. *)
        let open Protocol in
        Lwt.pick [Lwt_unix.timeout 1.0;
                  create_listener ~max_retries ~event_cb:begin function
                    | Event Attached { serial_number = s; connection_speed = _;
                                       connection_type = _; product_id = _;
                                       location_id = _; device_id = d; } ->
                      Hashtbl.add devices d s |> Lwt.return
                    | Event Detached d ->
                      Hashtbl.remove devices d |> Lwt.return
                    | _ -> Lwt.return_unit
                  end]
      with
        Lwt_unix.Timeout ->
        if do_daemonize then begin
          Lwt_daemon.daemonize ~syslog:true ();
          create_pid_file ()
        end;

        Lwt.async begin fun () ->
          Hashtbl.fold begin fun device_id_key udid_value accum ->
            try
              (Hashtbl.find device_mapping udid_value,
               device_id_key,
               udid_value) :: accum
            with
              Not_found ->
              Lwt_log.ign_info_f
                "Device with udid: %s expected but wasn't connected" udid_value;
              accum
          end
            devices [] |> Lwt_list.iter_s do_tunnel
        end;
        fst (Lwt.wait ())

    end

end
