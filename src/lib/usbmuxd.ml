open Lwt

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

    let rec do_read () =
      Lwt_io.read_line_opt ic >>= (function
          | None -> return ()
          | Some msg -> Lwt_io.printl msg) >>= do_read
    in
    do_read ()

end
