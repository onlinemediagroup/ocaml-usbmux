open Lwt
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

  let header_length = 16

  let do_read_header i_chan =
    i_chan |> Lwt_io.atomic begin fun ic ->
      Lwt_io.LE.read_int32 ic >>= fun raw_count ->
      Lwt_io.LE.read_int32 ic >>= fun raw_version ->
      Lwt_io.LE.read_int32 ic >>= fun raw_request ->
      Lwt_io.LE.read_int32 ic >|= fun raw_tag ->
      Int32.(to_int raw_count, to_int raw_version, to_int raw_request, to_int raw_tag)
    end

  (** Highly advised to only change value of version of default values *)
  let do_write_header ?(version=Plist) ?(request=8) ?(tag=1) ~total_len o_chan =
    o_chan |> Lwt_io.atomic begin fun oc ->
      (List.map Int32.of_int [total_len; if version = Plist then 1 else 0; request; tag])
      |> Lwt_list.iter_s (Lwt_io.LE.write_int32 oc)
    end

  let listen_message =
    Plist.(Dict [("MessageType", String "Listen");
                 ("ClientVersionString", String "ocaml-usbmux");
                 ("ProgName", String "ocaml-usbmux")]
           |> make)

  let log_reply = function
    | Result Success -> Lwt_log.info (colored_message "Listening for iDevices")
    | Result Device_requested_not_connected ->
      Lwt_log.info (colored_message ~m_color:T.Red "Device requested was not connected")
    | Result Port_requested_not_available ->
      Lwt_log.info (colored_message ~m_color:T.Red "Port requested was not available")
    | Result Malformed_request ->
      Lwt_io.printl (colored_message ~m_color:T.Red "Malformed request, check the request")
    (* Lazy, add rest here *)
    | _ -> return ()

  let parse_reply raw_reply =
    let open Yojson.Basic in
    let handle = Plist.parse_dict raw_reply in
    match (Util.member "MessageType" handle) |> Util.to_string with
    | "Result" -> (match (Util.member "Number" handle) |> Util.to_int with
        | 0 -> Result Success |> return
        | 2 -> Result Device_requested_not_connected |> return
        | 3 -> Result Port_requested_not_available |> return
        | 5 -> Result Malformed_request |> return
        | n -> Lwt.fail (Unknown_reply (Printf.sprintf "Unknown result code: %d" n)))
    | "Attached" ->
      Event (Attached
               {serial_number = Util.member "SerialNumber" handle |> Util.to_string;
                connection_speed = Util.member "ConnectionSpeed" handle |> Util.to_int;
                connection_type = Util.member "ConnectionType" handle |> Util.to_string;
                product_id = Util.member "ProductID" handle |> Util.to_int;
                location_id = Util.member "LocationID" handle |> Util.to_int;
                device_id = Util.member "DeviceID" handle |> Util.to_int ;})
      |> return
    | "Detached" ->
      Event (Detached (Util.member "DeviceID" handle |> Util.to_int))
      |> return
    | otherwise -> Lwt.fail (Unknown_reply otherwise)

  let handle r () = match r with
    (* Come back to this *)
    | Result (Success | Device_requested_not_connected) -> return ()
    | Event Attached { serial_number; connection_speed; connection_type;
                       product_id; location_id; device_id; } ->
      Lwt_io.printlf "Device %d with serial number: %s connected" device_id serial_number
    | Event Detached d ->
      Lwt_io.printlf "Device %d disconnected" d
    | _ -> return ()

  let create_listener debug =
    let total_len = (String.length listen_message) + header_length in
    Lwt_io.with_connection (Unix.ADDR_UNIX "/var/run/usbmuxd") begin fun (ic, oc) ->
      (* Send the header for our listen message *)
      do_write_header ~total_len oc >>= fun () ->
      (* Send the listen message body, aka the Plist *)
      Lwt_io.write_from_string oc listen_message 0 (String.length listen_message) >>= fun c ->
      Lwt_log.info
        (Printf.sprintf "Needed to write: %d, actually wrote: %d" (total_len - header_length) c
         |> colored_message) >>= fun () ->

      (* Read back the other side's header message *)
      do_read_header ic >>= fun (msg_len, version, request, tag) ->

      Lwt_log.info
        (colored_message ~m_color:T.Green
           (Printf.sprintf
              "Len: %d, Version: %d, Request: %d, Tag: %d"
              msg_len version request tag)) >>= fun () ->

      let buffer = Bytes.create (msg_len - header_length) in

      Lwt_io.read_into_exactly ic buffer 0 (msg_len - 16) >>= fun () ->

      (* We know how long the message length ought to be, so let's just read that much *)
      let rec forever () =
        do_read_header ic >>= fun (msg_len, version, request, tag) ->
        Lwt_log.info
          (colored_message ~m_color:T.Green
             (Printf.sprintf
                "Len: %d, Version: %d, Request: %d, Tag: %d"
                msg_len version request tag)) >>= fun () ->
        let buffer = Bytes.create (msg_len - header_length) in
        Lwt_io.read_into_exactly ic buffer 0 (msg_len - header_length) >>= fun () ->
        if debug then print_endline (Plist.parse_dict buffer |> B.pretty_to_string);
        buffer |> parse_reply >>= fun reply ->
        if debug then log_reply reply >>= handle reply >>= forever
        else handle reply () >>= forever
      in
      forever ()
    end

end

module Relay = struct

  let handle_signals sock =
    ignore begin Sys.(signal sigint (Signal_handle begin fun _ ->
        Lwt_unix.unix_file_descr sock |> Unix.close;
        exit 0
      end))
    end

  (* Note: PortNumber must be network-endian, so it gets byte swapped here *)
  let connect_message device_id port =
    Plist.((Dict [("MessageType", String "Connect");
                  ("ClientVersionString", String "ocaml-usbmux");
                  ("ProgName", String "ocaml-usbmux");
                  ("DeviceID", Integer device_id);
                  ("PortNumber", Integer (byte_swap_16 port))])
           |> make)

  let handle_connection (client_sock, client_info) =
    let (ic, oc) =
      Lwt_io.of_fd ~mode:Lwt_io.Input client_sock,
      Lwt_io.of_fd ~mode:Lwt_io.Output client_sock
    in
    print_endline "Someone connected";
    let rec read_all () =
      (* Here need to pass off to the local socket *)
      try%lwt
        Lwt_io.read_line ic >>= Lwt_io.printl >>= read_all
      (* When other side disconnected but we don't want to kill the
         server, just loops again. See keep_listening *)
      with End_of_file ->
        Lwt_io.printl "ended"
    in
    read_all ()

  let create verbose udid port_pairs =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    handle_signals sock;
    bind sock (ADDR_INET(Unix.inet_addr_loopback, 2000));
    listen sock 20;
    let rec keep_listening on_socket () =
      accept on_socket >>= handle_connection >>= keep_listening on_socket
    in
    Lwt_log.info (colored_message "Started TCP routing server") >>= keep_listening sock

end
