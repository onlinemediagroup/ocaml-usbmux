open Lwt.Infix
open Cmdliner

let be_verbose =
  let doc = "Output Debugging Info" in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let forward_connection_file =
  let open Arg in
  let doc = "Simple file mapping udid to ports, expecting a file \
             like 123gfdgefrgt234:2000"
  in
  value & opt (some non_dir_file) None & info ["m"; "mappings"] ~doc

let do_daemonize =
  let open Arg in
  let doc = "Whether$(b, $(tname)) should run as a daemon" in
  value & flag & info ["d";"daemonize"] ~doc

let do_exit =
  let open Arg in
  let doc = "Gracefully exit current running relay" in
  value & flag & info ["e";"exit"] ~doc

let retry_count =
  let open Arg in
  let doc = "How many times to retry action" in
  value & opt int 3 & info ["t"; "tries"] ~doc

let reload_mapping =
  let open Arg in
  let doc = "Stop running threads and reload the mappings \
             from the original mapping file path."
  in
  value & flag & info ["r";"reload"] ~doc

let status =
  let open Arg in
  let doc = "Pretty print json of currently tunneled devices" in
  value & flag & info ["s";"status"] ~doc

let show_status () =
  let open Cohttp_lwt_unix in
  let colorize = Usbmux.colored_message ~with_time:false in
  (try
     (Client.get Usbmux.Relay.status_server_query >>= fun (_, body) ->
      Cohttp_lwt_body.to_string body >>= fun s ->
      (Printf.sprintf "%s\n%s"
         ("Current actively tunneled devices, \
           ssh into them with the port numbers printed below.\n\
           Example:" |> colorize ~message_color:Usbmux.T.White)
         ("\tssh root@localhost -p <some_port>" |> colorize ~message_color:Usbmux.T.Cyan)
      )
      |> Lwt_io.printl >>= fun () ->
      Yojson.Basic.(from_string s |> pretty_to_string) |> Lwt_io.printl)
     |> Lwt_main.run |> ignore
   with
     Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
     Usbmux.error_with_color "Couldn't get status, check if relay is running"
     |> prerr_endline);
  exit 0

let begin_program
    debug
    port_pairs
    do_daemonize
    max_retries
    do_reload_mapping
    do_status
    do_exit =
  (* Black magic for the entire running process *)
  if debug then Lwt_log.add_rule "*" Lwt_log.Info;

  if do_exit then Usbmux.Relay.(perform Shutdown);

  if do_reload_mapping then Usbmux.Relay.(perform Reload);

  if do_status then show_status ();

  let module P = Usbmux.Protocol in
  (* Now we start spinning up Lwt *)
  match port_pairs with
  | None ->
    let open P in
    Usbmux.Protocol.create_listener ~max_retries ~event_cb:begin function
      | P.Event P.Attached { serial_number = s; connection_speed = _;
                             connection_type = _; product_id = _; location_id = _;
                             device_id = d; } ->
        Lwt_io.printlf "Device %d with serial number: %s connected" d s
      | P.Event P.Detached d -> Lwt_io.printlf "Device %d disconnected" d
      | _ -> Lwt.return ()
    end
  | Some device_map ->
    Usbmux.Relay.begin_relay ~device_map ~max_retries do_daemonize

let entry_point =
  Term.(pure
          begin_program
        $ be_verbose
        $ forward_connection_file
        $ do_daemonize
        $ retry_count
        $ reload_mapping
        $ status
        $ do_exit )

let top_level_info =
  let doc = "Control TCP forwarding for iDevices" in
  let man = [`S "DESCRIPTION";
             `P "$(b, $(tname)) is a program for controlling \
                 the interface of ssh tcp iphone";
             `S "EXAMPLES";
             `P "1) See with realtime updates what devices are connected \
                 This will start up gandalf in listen mode, that is it \
                 will print out whenver a device connects or disconnects";
             `Pre "$(b, $(tname))";
             `P "2) Start with a mapping file which is of the form \
                 <udid>:<port>, the # character starts comments.";
             `Pre "$(b, $(tname)) -m mapping_file";
             `P "2.1) Daemonize $(b, $(tname)) with the -d flag. *NOTE*: You \
                 might need to end up doing that under sudo as $(b, $(tname)) \
                 needs to make a pid file under /var/run. If daemonizing \
                 is failing, try running as root. After daemonzing, \
                 check the system log for debugging info";
             `P "3) See a pretty JSON representation of devices and \
                 their ports that are currently connected.";
             `Pre "$(b, $(tname)) -s";
             `P "4) Reload $(b, $(tname)) with a new set of mappings";
             `Pre "$(b, $(tname)) -r";
             `S "AUTHOR";
             `P "Edgar Aroutiounian"]
  in
  Term.info "gandalf" ~version:"0.2" ~doc ~man

let () =
  match Term.eval (entry_point, top_level_info) with
  | `Ok program -> Lwt_main.run program
  | `Error _ -> prerr_endline "Unhandled error"
  | _ -> ()
