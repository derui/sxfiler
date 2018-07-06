(** Rpc_connection handle request and response on Websocket as Lwt stream.
    This module defines global connection to be able to use other module to send
    frame.
*)
open Rpc_connection_abbrev

(** Type of Rpc_connection. *)
type t

(** [write_output t ~frame] writes a [frame] to peer on websocket connection.
    This function calls writer given with calling {!connect}.
*)
val write_output : t -> frame:W.Frame.t option -> unit

(** [process_input t ~f] handles input of [t] on websocket connection with [f] *)
val process_input : t -> f:(W.Frame.t -> unit Lwt.t) -> unit Lwt.t

(** [push_input t ~frame] writes a [frame] to peer on websocket connection. *)
val push_input : t -> frame:W.Frame.t option -> unit

(** [with_conn f] execute [f] if connection already created. If does not exists connection,
    do nothing this.
*)
val with_conn : (t -> unit Lwt.t) -> unit Lwt.t

(** [make ()] makes new connection *)
val make : unit -> t

(** [connect t output_writer] connect from websocket to [t] with [output_writer].
    Connection is saved as global.
*)
val connect : t -> (W.Frame.t option -> unit) -> unit Lwt.t

(** [disconnect ()] kills global connection. Notice disconnected connection can not use after this. *)
val disconnect : unit -> unit Lwt.t

(** [default_input_handler t f] handles frame [f] with default behavior for Websocket. *)
val default_input_handler : t -> W.Frame.t -> unit Lwt.t
