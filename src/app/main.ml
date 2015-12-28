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
  let doc = "How many times to retry action" in
  value & opt int 3 & info ["t"; "tries"] ~doc

let reload_mapping =
  let open Arg in
  let doc = "Stop running threads and reload the mappings" in
  value & flag & info ["r";"reload"] ~doc

let begin_program
    debug
    port_pairs
    do_daemonize
    max_retries
    do_reload_mapping =
  (* Black magic for the entire running process *)
  if debug then Lwt_log.add_rule "*" Lwt_log.Info;

  if do_reload_mapping then Usbmux.Relay.reload_mapping ();

  let module P = Usbmux.Protocol in
  (* Now we start spinning up Lwt *)
  match port_pairs with
  | None ->
    let open P in
    Usbmux.Protocol.create_listener ~max_retries ~event_cb:begin function
      | P.Event P.Attached { serial_number = s; connection_speed = _; connection_type = _;
                             product_id = _; location_id = _; device_id = d; } ->
        Lwt_io.printlf "Device %d with serial number: %s connected" d s
      | P.Event P.Detached d -> Lwt_io.printlf "Device %d disconnected" d
      | _ -> Lwt.return ()
    end
  | Some device_map -> Usbmux.Relay.begin_relay ~device_map ~max_retries do_daemonize

let entry_point =
  Term.(pure
          begin_program
        $ be_verbose
        $ forward_connection_file
        $ do_daemonize
        $ retry_count
        $ reload_mapping)

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
  Term.info "gandalf" ~version:"0.2" ~doc ~man

let () =
  match Term.eval (entry_point, top_level_info) with
  | `Ok program -> Lwt_main.run program
  | `Error _ -> prerr_endline "Unhandled error"
  | _ -> ()
