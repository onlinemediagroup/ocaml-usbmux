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

let colored_message ?(t_color=T.Yellow) ?(m_color=T.Blue) ?(with_time=true) str =
  let just_time = T.sprintf [T.Foreground t_color] "%s " (time_now ()) in
  let just_message = T.sprintf [T.Foreground m_color] "%s" str in
  if with_time then just_time ^ just_message else just_message

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

  let handle_reply = function
    | Result Success -> Lwt_log.info (colored_message "Listening for iDevices")
    | Result Device_requested_not_connected ->
      Lwt_log.info (colored_message ~m_color:T.Red "Device requested was not connected")
    | Result Port_requested_not_available ->
      Lwt_log.info (colored_message ~m_color:T.Red "Port requested was not available")
    | Result Malformed_request ->
      Lwt_log.info (colored_message ~m_color:T.Red "Malformed request, check the request")
    (* Lazy, add rest here *)
    | _ -> Lwt.return ()

  let parse_reply raw_reply () =
    let open Yojson.Basic in
    let handle = Plist.parse_dict raw_reply in
    match (Util.member "MessageType" handle) |> Util.to_string with
    | "Result" -> (match (Util.member "Number" handle) |> Util.to_int with
        | 0 -> Result Success |> Lwt.return
        | 2 -> Result Device_requested_not_connected |> Lwt.return
        | 3 -> Result Port_requested_not_available |> Lwt.return
        | 5 -> Result Malformed_request |> Lwt.return
        | n -> Lwt.fail (Unknown_reply (Printf.sprintf "Unknown result code: %d" n)))
    | "Attached" ->
      Event (Attached
               {serial_number = Util.member "SerialNumber" handle |> Util.to_string;
                connection_speed = Util.member "ConnectionSpeed" handle |> Util.to_int;
                connection_type = Util.member "ConnectionType" handle |> Util.to_string;
                product_id = Util.member "ProductID" handle |> Util.to_int;
                location_id = Util.member "LocationID" handle |> Util.to_int;
                device_id = Util.member "DeviceID" handle |> Util.to_int ;})
      |> Lwt.return
    | "Detached" ->
      Event (Detached (Util.member "DeviceID" handle |> Util.to_int))
      |> Lwt.return
    | otherwise -> Lwt.fail (Unknown_reply otherwise)

  let handle ?conn_table show_connections r () = match r with
    | Result (Success | Device_requested_not_connected) -> ()
    | Event Attached { serial_number = s; connection_speed = _; connection_type = _;
                       product_id = _; location_id = _; device_id = d; } ->
      (match conn_table with None -> () | Some t -> Hashtbl.add t d s);
      if show_connections
      then Printf.sprintf "Device %d with serial number: %s connected" d s
           |> print_endline
    | Event Detached d ->
      (match conn_table with None -> () | Some t -> Hashtbl.remove t d);
      if show_connections
      then Printf.sprintf "Device %d disconnected" d |> print_endline
    | _ -> ()

  let create_listener ?conn_table debug show_connections () =
    let total_len = (String.length listen_message) + header_length in
    Lwt_io.with_connection (Unix.ADDR_UNIX "/var/run/usbmuxd") begin fun (ic, oc) ->
      (* Send the header for our listen message *)
      write_header ~total_len oc >>= fun () ->
      (* Send the listen message body, aka the Plist *)
      Lwt_io.write_from_string oc listen_message 0 (String.length listen_message) >>= fun _ ->

      (* Read back the other side's header message *)
      read_header ic >>= fun (msg_len, _, _, _) ->

      let buffer = Bytes.create (msg_len - header_length) in

      Lwt_io.read_into_exactly ic buffer 0 (msg_len - 16) >>= fun () ->

      (* We know how long the message length ought to be, so let's just read that much *)
      let rec forever () =
        read_header ic >>= fun (msg_len, _, _, _) ->
        let buffer = Bytes.create (msg_len - header_length) in
        Lwt_io.read_into_exactly ic buffer 0 (msg_len - header_length) >>= fun () ->
        parse_reply buffer () >>= fun reply ->
        if debug
        then handle_reply reply >|= handle ?conn_table show_connections reply >>= forever
        else handle show_connections reply () |> Lwt.return >>= forever
      in
      forever ()
    end

end

module Relay = struct

  type conn_t = Connected | Disconnected

  let string_of_c_event = function
    | Connected -> "connected"
    | Disconnected -> "disconnected"

  let log_client c_sockaddr c_event =
    let open Unix in
    let c = string_of_c_event c_event in
    (match c_sockaddr with
     | ADDR_UNIX s ->
       Printf.sprintf "Client %s %s" s c
     | ADDR_INET (i, p) ->
       Printf.sprintf "Client %s on port %d %s" (string_of_inet_addr i) p c)
    |> colored_message
    |> Lwt_log.info

  (* Note: PortNumber must be network-endian, so it gets byte swapped here *)
  let connect_message ~device_id ~device_port =
    Plist.((Dict [("MessageType", String "Connect");
                  ("ClientVersionString", String "ocaml-usbmux");
                  ("ProgName", String "ocaml-usbmux");
                  ("DeviceID", Integer device_id);
                  ("PortNumber", Integer (byte_swap_16 device_port))])
           |> make)

  let echo ic oc = Lwt_io.(write_chars oc (read_chars ic))

  let running_tunnel d_id debug (tcp_ic, tcp_oc) () =
    Lwt_io.with_connection Protocol.usbmuxd_address begin fun (mux_ic, mux_oc) ->
      let msg = connect_message ~device_id:d_id ~device_port:22 in
      let do_relay () =

        let open Protocol in
        write_header ~total_len:(msg_length msg) mux_oc >>= fun () ->
        Lwt_io.write_from_string_exactly mux_oc msg 0 (String.length msg) >>= fun () ->
        (* Read the reply, should be good to start just raw piping *)
        read_header mux_ic >>= fun (msg_len, _, _, _) ->
        let buffer = Bytes.create (msg_len - header_length) in
        Lwt_io.read_into_exactly mux_ic buffer 0 (msg_len - 16) >>=
        parse_reply buffer >>= handle_reply >>= fun () ->
        echo tcp_ic mux_oc <&> echo mux_ic tcp_oc

      in
      [Protocol.create_listener debug false; do_relay]
      |> Lwt_list.iter_p (fun g -> g ())

    end

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

  let begin_relay debug m_file retry_count do_daemonize =
    Lwt_io.set_default_buffer_size 32768;
    let max_try_count =
      match retry_count with None -> 3 | Some i -> assert (i > 0 && i < 20); i
    in
    load_mappings m_file >>= fun device_mapping ->
    let devices_table = Hashtbl.create 12 in
    try%lwt
      Lwt.pick [Lwt_unix.timeout 1.0;
                Protocol.create_listener ~conn_table:devices_table true false ()]
    with
      Lwt_unix.Timeout ->
      Hashtbl.fold begin fun device_id_key udid_value accum ->
        try
          (Hashtbl.find device_mapping udid_value, device_id_key) :: accum
        with
          Not_found ->
          Lwt_log.ign_info_f
            "Device with udid: %s expected to be connected but wasn't" udid_value;
          accum
      end
        devices_table []
      |> fun port_and_d_ids ->
      if do_daemonize then Lwt_daemon.daemonize ();
      Lwt.return port_and_d_ids >>= Lwt_list.iter_p begin fun (port, device_id) ->
        let server_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
        let _ =
          Lwt_io.establish_server server_address begin fun with_tcp_client ->
            let rec do_start retry_count () =
              if retry_count = max_try_count
              then colored_message ~m_color:T.Red (Printf.sprintf "Tried %d times, gave up" retry_count)
                   |> Lwt_io.printl
              else begin Lwt.catch
                  (running_tunnel device_id debug with_tcp_client)
                  Unix.(function
                        Unix_error(ECONNREFUSED, _, _) ->
                        retry_count
                        |> Lwt_log.info_f "Attempt %d, could not connect to usbmuxd, check if its running" >>=
                        do_start (retry_count + 1)
                      (* Handle other cases of interest, maybe EBADF? *)
                      | _ -> do_start (retry_count + 1) ())
              end
            in
            do_start 1 () |> Lwt.ignore_result
          end
        in
        let (t, _) = Lwt.wait () in
        t
      end

end
