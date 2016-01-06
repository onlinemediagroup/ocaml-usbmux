module T = ANSITerminal

(** What platform name *)
type platform = Linux | Darwin

(** Shell colored string with possibility of timestamp *)
val colored_message :
  ?time_color:T.color ->
  ?message_color:T.color -> ?with_time:bool -> string -> string

(** Default coloring for a error string *)
val error_with_color : string -> string

val log_info_bad : ?exn:exn -> string -> unit Lwt.t

val log_info_success : string -> unit Lwt.t

(** Get the current platform *)
val platform : unit -> platform Lwt.t

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
  type event = Attached of device_t | Detached of device_id

  (** Simple type alias for integer *)
  and device_id = int

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
    ?event_cb:(msg_t -> unit Lwt.t) -> max_retries:int -> unit Lwt.t

end

(** Provides functions to create and manipulate tunnedled relays *)
module Relay : sig

  (** Actions that can be performed on running relays *)
  type action = Shutdown | Reload

  (** Create a relay with option to daemonize. *)
  val begin_relay :
    device_map:Lwt_io.file_name -> max_retries:int -> bool -> unit Lwt.t

  (** Perform an action on a relay *)
  val perform : action -> unit

  (** Get JSON that that describes the currently tunneled devices *)
  val status : unit -> Yojson.Basic.json Lwt.t

end
