(** Path to relay's running pid file *)
val pid_file : string

module Logging : sig

  type log_opts = { log_conns : bool;
                    log_async_exn : bool;
                    log_plugged_inout : bool;
                    log_everything_else : bool; }

end

(** Module containing types definitions and functions for
    communicating with usbmuxd *)
module Protocol : sig

  (** A plist can be either binary or XML *)
  type msg_version_t = Binary | Plist

  (** Result code after trying to establish a connection for a
      device *)
  type conn_code =
      Success
    | Device_requested_not_connected
    | Port_requested_not_available
    | Malformed_request

  (** A device event with associated metadata *)
  type event = Attached of device_t | Detached of int

  (** High level metadata about the device connection *)
  and device_t = {
    serial_number : string;
    connection_speed : int;
    connection_type : string;
    product_id : int;
    location_id : int;
    device_id : int;
  }

  (** Reply from usbmuxd, could be an event or reply to a query *)
  type msg_t = Result of conn_code | Event of event

  exception Unknown_reply of string

  (** Creates a listener waiting for events, ie connections and
      disconnections *)
  val create_listener :
    ?event_cb:(msg_t -> unit Lwt.t) -> unit -> unit Lwt.t

end

(** Provides functions to create and manipulate tunneled relays *)
module Relay : sig

  (** Actions that can be performed on running relays *)
  type action = Shutdown | Reload

  (** Create a relay, last int is retries *)
  val begin_relay :
    ?log_opts:Logging.log_opts ->
    ?stats_server:bool ->
    tunnel_timeout:int ->
    device_map:Lwt_io.file_name ->
    unit Lwt.t

  (** Perform an action on a relay *)
  val perform : action -> unit

  (** Get JSON that that describes the currently tunneled devices *)
  val status : unit -> Yojson.Basic.json Lwt.t

end
