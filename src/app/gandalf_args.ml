open Cmdliner

let be_verbose =
  let doc =
    "Loudest logging setting, equivalent to turning on \
     log_connections, log_async_exceptions, \
     log_plugged_action and log_everything_else."
  in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let ignore_all_unix_errors =
  let doc = "Ignore all Unix exceptions, most likely raised by Usbmuxd. \
             Warning: this may hide errors"
  in
  Arg.(value & flag & info ["ignore_all_unix_exceptions"] ~doc)

let forward_connection_file =
  let doc =
    "A JSON based mapping from udid to local ports to forward, \
     read further down for an example mapping."
  in
  Arg.(value & opt (some non_dir_file) None & info ["m"; "mappings"] ~doc)

let do_daemonize =
  let doc = "Whether $(b,$(tname)) should run as a daemon." in
  Arg.(value & flag & info ["d"; "daemonize"] ~doc)

let do_exit =
  let doc = "Gracefully exit currently running tunnels." in
  Arg.(value & flag & info ["e"; "exit"] ~doc)

let tunneling_timeout =
  let doc = "If provided then how many seconds $(b,$(tname)) \
             will wait on inactivity on the tunnel before \
             closing the tunnel."
  in
  Arg.(value & opt (some int) None & info ["o"; "timeout"] ~doc)

let reload_mapping =
  let doc = "Stop running threads and reload the mappings \
             from the original mapping file path."
  in
  Arg.(value & flag & info ["r"; "reload"] ~doc)

let status_server_port =
  let doc = "Port that $(b,$(tname))'s status server will use" in
  Arg.(value & opt (some int) (Some 5000) & info ["status_port"] ~doc)

let bind_host =
  let doc =
    "Host that tunnels will bind to/listen on" in
  Arg.(value & opt (some string) None & info ["l"; "bind_host"] ~doc)

let status =
  let doc = "Metadata and pretty print json of \
             currently tunneled devices."
  in
  Arg.(value & flag & info ["s"; "status"] ~doc)

let log_connections =
  let doc =
    "Log individual tunnelings, this includes relay \
     open, close and can be quite noisy."
  in
  Arg.(value & flag & info ["log_connections"] ~doc)

let log_async_exceptions =
  let doc = "Log asynchronous exceptions." in
  Arg.(value & flag & info ["log_async_exceptions"] ~doc)

let log_plugged_action =
  let doc = "Log when a device is plugged in and plugged out." in
  Arg.(value & flag & info ["log_plugged_action"] ~doc)

let log_everything_else =
  let doc = "Log when everything other than \
             log_{connections, async_exceptions, plugged_actions}"
  in
  Arg.(value & flag & info ["log_everything_else"] ~doc)
