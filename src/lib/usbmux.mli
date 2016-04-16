(**
   Usbmux is the library that powers {b gandalf}, the program that
   lets you forward local ports to ports on an iDevice effectively
   letting you {b ssh} into it and run other network programs.
*)

(** Path to relay's running pid file *)
val pid_file : string

(** [Logging] provides a type definition for logging options *)
module Logging : sig

  (** Logging options *)
  type log_opts = {
    log_conns : bool;
    (** Whether to log individual tunnels, both connecting and
        disconnecting; this can get quite noisy. *)
    log_async_exn : bool;
    (** Whether to log asynchronous exceptions. *)
    log_plugged_inout : bool;
    (** Whether to log when devices are plugged in or out. *)
    log_everything_else : bool;
    (** Whether to log all other events. *)
  }

end

(** Module containing types definitions and functions for
    communicating with {b usbmuxd}. *)
module Protocol : sig

  (** A plist can be either binary or XML. *)
  type msg_version_t = Binary | Plist

  (** Result code after trying to establish a connection for a
      device, variant names are self-documenting. *)
  type conn_code =
      Success
    | Device_requested_not_connected
    | Port_requested_not_available
    | Malformed_request

  (** A device event with associated metadata. *)
  type event = Attached of device_t | Detached of int

  (** High level self documenting metadata about the device
      connection. *)
  and device_t = {
    serial_number : string;
    connection_speed : int;
    connection_type : string;
    product_id : int;
    location_id : int;
    device_id : int;
  }

  (** Reply from usbmuxd, could be an event or reply to a query. *)
  type msg_t = Result of conn_code | Event of event

  type exn += Unknown_reply of string

  (** Creates a listener waiting for events, ie connections and
      disconnections. [create_listener ()] creates a Lwt thread, the
      optional callback is your chance to handle the event. *)
  val create_listener :
    ?event_cb:(msg_t -> unit Lwt.t) -> unit -> unit Lwt.t

end

(** Provides functions to create and manipulate tunneled relays *)
module Relay : sig

  (** Actions that can be performed on running relays *)
  type action = Shutdown | Reload

  (** Exception raised when the mapping file is incorrectly made,
      probably a JSON error *)
  type exn += Mapping_file_error of string

  (** Create tunnels requested in a mapping file
      provided.

      [make_tunnels ~stats_server:true tunnel_timeout:3000 ~device_map:"some/path/mapping"]

      creates the tunnels requested for in the {b device_map} and creates a
      HTTP status server that is accessible at localport 5000. *)
  val make_tunnels :
    ?log_opts:Logging.log_opts ->
    ?stats_server:int ->
    ?tunnel_timeout:int ->
    device_map:Lwt_io.file_name ->
    unit Lwt.t

  (** Perform an action on running tunnels. *)
  val perform : action -> unit

  (** Get JSON that describes the currently ready tunnels. *)
  val status : unit -> Yojson.Basic.json Lwt.t

end
