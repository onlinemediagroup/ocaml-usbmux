open Lwt

(* external json_of_plist  *)

(* let devices = Hashtbl.create 12 *)

let address = if Sys.win32 then "27015" else "/var/run/usbmuxd"

let byte_swap_16 value =
   ((value land 0xFF) lsl 8) lor ((value lsr 8) land 0xFF)

module Protocol = struct

  type header = { length : Stdint.uint32;
                  version : Stdint.uint32;
                  tag : Stdint.uint32; }

  type message_t = Listen | Connect

  type payload = { message_t : message_t ;
                   client_version : string;
                   program_name : string; }

  (* let pack { message_t; client_version; program_name } = *)


end
