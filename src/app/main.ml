open Lwt.Infix
open Cmdliner

let do_listen =
  let doc = "Listen for connections" in
  Arg.(value & flag & info ["l"; "listen"] ~doc)

let forward_connection =
  let doc = "What port to forward connections to" in
  Arg.(value & opt string "5000" & info ["p";"port"] ~doc)

let begin_program
    do_listen
    forward_connection =
  (* This is the port *)
  (* print_endline forward_connection; *)
  if do_listen then (Usbmux.Protocol.create_listener ())
  (* Here need to start forwarding to other ports *)
  else Lwt.return ()

let entry_point =
  Term.(pure
          begin_program
        $ do_listen
        $ forward_connection)

let top_level_info =
  let doc = "Control TCP forwarding for sshing iDevices" in
  let man = [`S "DESCRIPTION";
             `P "$(b, $(tname)) is a program for controlling \
                 the interface of ssh tcp iphone";
             `S "AUTHOR";
             `P "Edgar Aroutiounian"]
  in
  Term.info "gandalf" ~version:"0.1" ~doc ~man

let () =
  match Term.eval (entry_point, top_level_info) with
  | `Ok a ->
    Lwt_log.add_rule "*" Lwt_log.Info;
    Lwt_main.run a
  | `Error _ -> prerr_endline "Something errored"
  | _ -> ()
