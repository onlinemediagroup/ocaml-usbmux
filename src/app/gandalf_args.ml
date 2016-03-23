open Cmdliner

let be_verbose =
  let doc =
    "Loudest logging setting, equivalent to turning on \
     log_connections, log_async_exceptions and \
     log_plugged_action"
  in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let forward_connection_file =
  let doc = "Simple file mapping udid to ports, expecting a file \
             like 123gfdgefrgt234:2000:22"
  in
  Arg.(value & opt (some non_dir_file) None & info ["m"; "mappings"] ~doc)

let do_daemonize =
  let doc = "Whether $(b,$(tname)) should run as a daemon" in
  Arg.(value & flag & info ["d";"daemonize"] ~doc)

let do_exit =
  let doc = "Gracefully exit current running relay" in
  Arg.(value & flag & info ["e";"exit"] ~doc)

let retry_count =
  let doc = "How many times to retry action" in
  Arg.(value & opt int 3 & info ["t"; "tries"] ~doc)

let tunneling_timeout =
  let doc = "How many seconds $(b,$(tname)) will wait on \
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

let log_connections =
  let doc =
    "Log individual tunnelings, this includes relay \
     open, close and can be quite noisy"
  in
  Arg.(value & flag & info ["log_connections"] ~doc)

let log_async_exceptions =
  let doc = "Log asynchronous exceptions" in
  Arg.(value & flag & info ["log_async_exceptions"] ~doc)

let log_plugged_action =
  let doc = "Log when a device is plugged in and plugged out" in
  Arg.(value & flag & info ["log_plugged_action"] ~doc)

let log_everything_else =
  let doc = "Log when everything other than \
             log_{connections,async_exceptions,plugged_actions}"
  in
  Arg.(value & flag & info ["log_everything_else"] ~doc)
