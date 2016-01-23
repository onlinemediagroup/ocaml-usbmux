open Lwt.Infix
open Cmdliner

module P = Usbmux.Protocol
module R = Usbmux.Relay

let be_verbose =
  let doc = "Output Debugging Info" in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let forward_connection_file =
  let doc = "Simple file mapping udid to ports, expecting a file \
             like 123gfdgefrgt234:2000:22"
  in
  Arg.(value & opt (some non_dir_file) None & info ["m"; "mappings"] ~doc)

let do_daemonize =
  let doc = "Whether$(b, $(tname)) should run as a daemon" in
  Arg.(value & flag & info ["d";"daemonize"] ~doc)

let do_exit =
  let doc = "Gracefully exit current running relay" in
  Arg.(value & flag & info ["e";"exit"] ~doc)

let retry_count =
  let doc = "How many times to retry action" in
  Arg.(value & opt int 3 & info ["t"; "tries"] ~doc)

let tunneling_timeout =
  let doc = "How many seconds $(b, $(tname)) will wait on \
             inactivity on the tunnel before closing the tunnel"
  in
  Arg.(value & opt int (60 * 5) & info ["o";"timeout"] ~doc)

let reload_mapping =
  let doc = "Stop running threads and reload the mappings \
             from the original mapping file path."
  in
  Arg.(value & flag & info ["r";"reload"] ~doc)

let status =
  let doc = "Pretty print json of currently tunneled devices" in
  Arg.(value & flag & info ["s";"status"] ~doc)

(* Ripped from cmdliner *)
let find_cmd cmds =
  let test, null = match Sys.os_type with
    | "Win32" -> "where", " NUL"
    | _ -> "type", "/dev/null"
  in
  let cmd c = Sys.command (Printf.sprintf "%s %s 1>%s 2>%s" test c null null) = 0 in
  try Some (List.find cmd cmds) with Not_found -> None

let show_status () =
  (* Ripped from cmdliner *)
  let pager =
    let cmds = ["less"; "more"] in
    let cmds = try (Sys.getenv "PAGER") :: cmds with Not_found -> cmds in
    let cmds = try (Sys.getenv "MANPAGER") :: cmds with Not_found -> cmds in
    find_cmd cmds
  in
  let colorize = Usbmux.colored_message ~with_time:false in
  Lwt_main.run begin
    try%lwt
      R.status () >>= fun as_json ->
      let uptime =
        Yojson.Basic.Util.member "uptime" as_json
        |> Yojson.Basic.Util.to_float
      in
      let payload = Yojson.Basic.Util.member "status_data" as_json in
      let msg =
        Printf.sprintf "%s\n%d %s\n%s\n%s"
          (Printf.sprintf "Uptime: Hours: %.2f Minutes: %.2f Seconds: %.2f"
             (uptime /. 60.0 /. 60.0)
             (uptime /. 60.0)
             (uptime))
          (Yojson.Basic.Util.to_list payload |> List.length)
          ("devices are tunneled, ssh into them with the port numbers printed \
            below.\nExample:" |> colorize ~message_color:Usbmux.T.White)
          ("\tssh root@localhost -p <some_port>"
           |> colorize ~message_color:Usbmux.T.Cyan)
          (Yojson.Basic.pretty_to_string payload)
      in
      match pager with
      | None -> Lwt_io.printl msg
      | Some p ->
        let f_name = Filename.temp_file "gandalf" "status" in
        (fun oc -> Lwt_io.write_from_string_exactly oc msg 0 (String.length msg))
        |> Lwt_io.with_file ~mode:Lwt_io.Output f_name >>= fun () ->
        Sys.command (Printf.sprintf "%s %s" p f_name) |> ignore;
        Lwt_unix.unlink f_name
    with Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
      Usbmux.error_with_color "Couldn't get status, check if gandalf is running"
      |> Lwt_io.printl
  end;
  exit 0

let begin_program
    debug
    port_pairs
    do_daemonize
    max_retries
    do_reload_mapping
    do_status
    tunnel_timeout
    do_exit =
  (* Black magic for the entire running process *)
  if debug then Lwt_log.add_rule "*" Lwt_log.Info;

  if do_exit then R.(perform Shutdown);

  begin
    if do_reload_mapping then
      try R.(perform Reload)
      with Sys_error _ ->
        (Usbmux.error_with_color
           (Printf.sprintf "Could not open pid file, are you sure gandalf was \
                            already running?")
         |> prerr_endline;
         exit 6)
  end;

  if do_status then show_status ();

  (* Now we start spinning up Lwt *)
  match port_pairs with
  | None ->
    if do_daemonize
    then Usbmux.error_with_color "Warning: Only in listen mode, not daemonizing"
         |> prerr_endline;
    P.(create_listener ~max_retries ~event_cb:begin function
        | Event Attached { serial_number = s; connection_speed = _;
                           connection_type = _; product_id = _; location_id = _;
                           device_id = d; } ->
          Lwt_io.printlf "Device %d with serial number: %s connected" d s
        | Event Detached d -> Lwt_io.printlf "Device %d disconnected" d
        | _ -> Lwt.return ()
      end)
  | Some device_map -> R.begin_relay ~tunnel_timeout ~device_map ~max_retries do_daemonize

let entry_point =
  Term.(pure
          begin_program
        $ be_verbose
        $ forward_connection_file
        $ do_daemonize
        $ retry_count
        $ reload_mapping
        $ status
        $ tunneling_timeout
        $ do_exit )

let top_level_info =
  let doc = "Control TCP forwarding for iDevices" in
  let man = [`S "DESCRIPTION";
             `P "$(b, $(tname)) lets you port forward local ports to specific \
                 ports on your jailbroken iDevices like iPhone, iPad, iTouch \
                 over USB; think about the ssh use-case.  You need to \
                 have the usbmuxd daemon running, on OS X this means you \
                 don't have to do anything but on Linux you need to \
                 install and run it.";
             `S "EXAMPLES";
             `P "1) See with realtime updates what devices are connected \
                 This will start up gandalf in listen mode, that is it \
                 will print out whenver a device connects or disconnects";
             `Pre "$(b, $(tname))";
             `P "2) Start with a mapping file which is of the form \
                 <udid>:<local_port>:<device_port>, the # character \
                 starts comments.";
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
             `P "5) Cleanly exit $(b, $(tname)), note this might require\
                 super user permissions.";
             `Pre "$(b, $(tname)) -e";
             `S "GUIDELINE";
             `P "Be sure to check your system log for valuable \
                 debugging information, especially with -v";
             `S "AUTHOR";
             `P "Edgar Aroutiounian"]
  in
  Term.info "gandalf" ~version:"0.8" ~doc ~man

let () =
  match Term.eval (entry_point, top_level_info) with
  | `Ok program -> Lwt_main.run program
  | _ -> ()
