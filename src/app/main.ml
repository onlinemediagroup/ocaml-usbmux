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
  value & pos_all (pair ~sep:':' int int) [] (info [] ~doc)

let do_daemonize =
  let open Arg in
  let doc = "Whether $(b, $(tname)) should run as a daemon" in
  value & flag & info ["d";"daemonize"] ~doc

let retry_count =
  let open Arg in
  let doc = "How many times to retry tunneling connections in \
             case they fail, defaults to 3"
  in
  value & opt (some int) None & info ["t"; "tries"] ~doc

let begin_program
    do_listen
    debug
    picked_udid
    port_pairs
    do_daemonize
    retry_count =
  (* Black magic for the entire running process *)
  if debug then Lwt_log.add_rule "*" Lwt_log.Info;

  if do_listen then Usbmux.Protocol.create_listener debug ()
  else Usbmux.Relay.begin_relay debug picked_udid port_pairs retry_count do_daemonize

let entry_point =
  Term.(pure
          begin_program
        $ do_listen
        $ be_verbose
        $ picked_udid
        $ forward_connection
        $ do_daemonize
        $ retry_count)

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
  | `Ok program -> Lwt_main.run program
  | `Error _ -> prerr_endline "Something errored"
  | _ -> ()
