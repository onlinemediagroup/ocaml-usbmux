open Lwt.Infix
open Cmdliner

let do_listen =
  let doc = "Listen for connections" in
  Arg.(value & flag & info ["l"; "listen"] ~doc)

let be_verbose =
  let doc = "Output Debugging Info" in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let picked_udid =
  let doc = "Specify device to connect to by UDID" in
  Arg.(value & opt (some string) None & info ["u"; "udid"] ~doc)

let forward_connection =
  let open Arg in
  let doc = "What ports to do forwarding on, example 22:5000" in
  value & opt (some (pair ~sep:':' int int)) None & info ["p";"port"] ~doc

let begin_program
    do_listen
    be_verbose
    picked_udid
    forward_connection =
  if be_verbose then Lwt_log.add_rule "*" Lwt_log.Info;
  match (do_listen, picked_udid, forward_connection) with
  | (true, _, _) -> Usbmux.Protocol.create_listener be_verbose
  | (_, picked_udid, forward_pair) ->
    Usbmux.do_start_relay picked_udid forward_pair

(* ssh root@localhost -p 2222   *)

let entry_point =
  Term.(pure
          begin_program
        $ do_listen
        $ be_verbose
        $ picked_udid
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
    Lwt_main.run a
  | `Error _ -> prerr_endline "Something errored"
  | _ -> ()
