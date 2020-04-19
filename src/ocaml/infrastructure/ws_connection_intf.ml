(** This module defines interface of connection for RPC. *)

module W = Websocket

module type S = sig
  type t
  (** Type of Rpc_connection. *)

  val write_output : t -> frame:W.Frame.t option -> unit
  (** [write_output t ~frame] writes a [frame] to peer on websocket connection. This function calls writer given with
      calling {!connect}. *)

  val process_input : t -> f:(W.Frame.t -> unit Lwt.t) -> unit Lwt.t
  (** [process_input t ~f] handles input of [t] on websocket connection with [f] *)

  val push_input : t -> frame:W.Frame.t option -> unit
  (** [push_input t ~frame] writes a [frame] to peer on websocket connection. *)

  val connect : t -> (W.Frame.t option -> unit) -> unit Lwt.t
  (** [connect t output_writer] connect from websocket to [t] with [output_writer]. Connection is saved as global. *)

  val disconnect : t -> unit Lwt.t
  (** [disconnect t] kills global connection. Notice disconnected connection can not use after this. *)

  val default_input_handler : t -> W.Frame.t -> unit Lwt.t
  (** [default_input_handler t f] handles frame [f] with default behavior. Handling frame with connection provided from
      this module will call when user do not have any behavior what you want. *)

  val is_closed : t -> bool
  (** [is_closed t] returns closed connection (called disconnect before) or not. *)
end

(** {!Instance} handles instance of connection *)
module type Instance = sig
  module Connection : S

  val instance : Connection.t
end
