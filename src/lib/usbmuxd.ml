open Lwt

external plist_to_json : string -> string = "json_string_of_plist"

let byte_swap_16 value =
  ((value land 0xFF) lsl 8) lor ((value lsr 8) land 0xFF)

module Protocol = struct

  let listen_message =
    (Plist.Dict [("MessageType", Plist.String "Listen");
                 ("ClientVersionString", Plist.String "node-usbmux");
                 ("ProgName", Plist.String "node-usbmux")])
    |> Plist.make

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

      Lwt_io.read ~count:(msg_len) ic >>= fun reply ->

      plist_to_json reply |> print_endline;

      forever ()
    in
    forever ()

end
