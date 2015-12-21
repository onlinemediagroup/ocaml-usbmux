open Cmdliner

let be_verbose =
  let doc = "Output Debugging Info" in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let forward_connection_file =
  let open Arg in
  let doc = "Simple file mapping udid to ports, expecting a file \
             like 123gfdgefrgt234:2000"
  in
  value & opt (some file) None & info ["m"; "mappings"] ~doc

let do_daemonize =
  let open Arg in
  let doc = "Whether$(b, $(tname)) should run as a daemon" in
  value & flag & info ["d";"daemonize"] ~doc

let retry_count =
  let open Arg in
  let doc = "How many times to retry tunneling connections in \
             case they fail, defaults to 3"
  in
  value & opt (some int) None & info ["t"; "tries"] ~doc

let begin_program
    debug
    port_pairs
    do_daemonize
    retry_count =
  (* Black magic for the entire running process *)
  if debug then Lwt_log.add_rule "*" Lwt_log.Info;

  match port_pairs with
  | None -> Usbmux.Protocol.create_listener debug true ()
  | Some file -> Usbmux.Relay.begin_relay debug file retry_count do_daemonize

let entry_point =
  Term.(pure
          begin_program
        $ be_verbose
        $ forward_connection_file
        $ do_daemonize
        $ retry_count)

let top_level_info =
  let doc = "Control TCP forwarding for sshing iDevices" in
  let man = [`S "DESCRIPTION";
             `P "$(b, $(tname)) is a program for controlling \
                 the interface of ssh tcp iphone";
             `S "USAGE";
             `P "if$(b, $(tname)) is invoked with no arguments then it begins \
                 in listen mode, which means it will just show the DeviceIDs as assigned \
                 by usbmuxd and the device's udid.";
             `S "AUTHOR";
             `P "Edgar Aroutiounian"]
  in
  Term.info "gandalf" ~version:"0.1" ~doc ~man

let () =
  match Term.eval (entry_point, top_level_info) with
  | `Ok program -> Lwt_main.run program |> ignore
  | `Error _ -> prerr_endline "Something errored"
  | _ -> ()
