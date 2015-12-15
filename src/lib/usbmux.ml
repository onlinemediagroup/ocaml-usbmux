open Lwt

let byte_swap_16 value =
  ((value land 0xFF) lsl 8) lor ((value lsr 8) land 0xFF)

module Protocol = struct

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

  let listen_message =
    (Plist.Dict [("MessageType", Plist.String "Listen");
                 ("ClientVersionString", Plist.String "ocaml-usbmux");
                 ("ProgName", Plist.String "ocaml-usbmux")])
    |> Plist.make

  let log_reply = function
    | Result Success -> Lwt_log.info "Listening for iDevices"
    | Result Device_requested_not_connected ->
      Lwt_log.info "Device requested was not connected"
    | Result Port_requested_not_available ->
      Lwt_log.info "Port requested was not available"
    | _ -> return ()

  let parse_reply handle =
    let open Yojson.Basic in
    match (Util.member "MessageType" handle) |> Util.to_string with
    | "Result" -> (match (Util.member "Number" handle) |> Util.to_int with
        | 0 -> Result Success
        | 2 -> Result Device_requested_not_connected
        | 3 -> Result Port_requested_not_available
        | 5 -> Result Malformed_request
        | _ -> assert false)
    | "Attached" ->
      Event (Attached {serial_number = Util.member "SerialNumber" handle |> Util.to_string;
                       connection_speed = Util.member "ConnectionSpeed" handle |> Util.to_int;
                       connection_type = Util.member "ConnectionType" handle |> Util.to_string;
                       product_id = Util.member "ProductID" handle |> Util.to_int;
                       location_id = Util.member "LocationID" handle |> Util.to_int;
                       device_id = Util.member "DeviceID" handle |> Util.to_int ;})
    | "Detached" ->
      Event (Detached (Util.member "DeviceID" handle |> Util.to_int))
    | otherwise ->
      print_endline otherwise;
      assert false

  let handle = function
    | Result Success -> Lwt_io.printl "got a success"
    | Result Device_requested_not_connected -> Lwt_io.printl "Device asked for isn't even connected"
    | Result Malformed_request -> Lwt_io.printl "Malformed request, check the request"
    | Event Attached { serial_number; connection_speed; connection_type;
                       product_id; location_id; device_id; } ->
      Lwt_io.printlf "Device %d with serial number: %s connected" device_id serial_number
    | Event Detached d ->
      Lwt_io.printlf "Device %d disconnected" d
    | _ -> return ()

  let create_listener () =
    let total_len = (String.length listen_message) + 16 in
    Lwt_io.open_connection (Unix.ADDR_UNIX "/var/run/usbmuxd") >>= fun (ic, oc) ->

    Lwt_io.LE.write_int32 oc (Int32.of_int total_len) >>= fun () ->
    Lwt_io.LE.write_int32 oc (Int32.of_int 1) >>= fun () ->
    Lwt_io.LE.write_int32 oc (Int32.of_int 8) >>= fun () ->
    Lwt_io.LE.write_int32 oc (Int32.of_int 1) >>= fun () ->

    Lwt_io.write_from_string oc listen_message 0 (String.length listen_message) >>= fun c ->
    Lwt_log.info ("Wrote " ^ string_of_int c) >>= fun () ->

    let rec forever () =
      (* TODO abstract into a function call *)
      Lwt_io.LE.read_int32 ic >>= fun c ->
      Lwt_io.LE.read_int32 ic >>= fun _ ->
      Lwt_io.LE.read_int32 ic >>= fun _ ->
      Lwt_io.LE.read_int32 ic >>= fun _ ->

      let msg_len = Int32.to_int c in

      Lwt_log.info (Printf.sprintf "Reply length %d\n" msg_len) >>= fun () ->

      Lwt_io.read ~count:(msg_len) ic >>= fun raw_reply ->

      (* Plist.parse_dict raw_reply |> Yojson.Basic.pretty_to_string |> print_endline; *)

      let reply = Plist.parse_dict raw_reply |> parse_reply in
      handle reply >>= forever
    in
    forever ()

end
