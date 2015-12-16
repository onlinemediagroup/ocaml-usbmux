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

let begin_program
    do_listen
    be_verbose
    picked_udid
    forward_connection =
  if be_verbose then Lwt_log.add_rule "*" Lwt_log.Info;

  let rec do_start retry_count =
    try%lwt
      match (do_listen, picked_udid, forward_connection) with
      | (true, _, _) ->
        Usbmux.Protocol.create_listener be_verbose
      | (_, picked_udid, pairs) ->
        Usbmux.Relay.begin_relay be_verbose picked_udid pairs
    with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      (* Maybe add logic to do the spawning, will need 2 more command
         line args *)
      if retry_count = 2
      then
        Printf.sprintf "Tried %d times, check if usbmuxd is running" (retry_count + 1)
        |> Usbmux.colored_message ~m_color:Usbmux.T.Red |> Lwt_io.printl
      else do_start (retry_count + 1)
    | otherwise -> Lwt_log.ign_error (Printexc.to_string otherwise) |> Lwt.return
  in
  do_start 0

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
  | `Ok program -> Lwt_main.run program
  | `Error _ -> prerr_endline "Something errored"
  | _ -> ()
