open Lwt

let () =
  Lwt_log.add_rule "*" Lwt_log.Info;
  Lwt_main.run (Usbmuxd.Protocol.create_listener ())
