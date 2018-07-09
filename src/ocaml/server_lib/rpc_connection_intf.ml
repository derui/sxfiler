(** This module defines interface of connection for RPC. *)

open Rpc_connection_abbrev

module type Connection = sig
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

  (** [connect t output_writer] connect from websocket to [t] with [output_writer].
      Connection is saved as global.
  *)
  val connect : t -> (W.Frame.t option -> unit) -> unit Lwt.t

  (** [disconnect t] kills global connection. Notice disconnected connection can not use after this. *)
  val disconnect : t -> unit Lwt.t

  (** [default_input_handler t f] handles frame [f] with default behavior.
      Handling frame with connection provided from this module will call when user do not have any
      behavior what you want.
  *)
  val default_input_handler : t -> W.Frame.t -> unit Lwt.t

  (** [is_closed t] returns closed connection (called disconnect before) or not. *)
  val is_closed: t -> bool
end

(** {!Instance} handles instance of connection *)
module type Instance = sig
  module Connection : Connection

  val instance : Connection.t
end
