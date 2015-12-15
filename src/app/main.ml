open Lwt.Infix
open Cmdliner

let () =
  Lwt_log.add_rule "*" Lwt_log.Info;
  Lwt_main.run (Usbmux.Protocol.create_listener ())
