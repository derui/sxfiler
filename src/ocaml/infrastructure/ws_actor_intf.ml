open Ws_abbrev

type receiver = W.Frame.t -> [ `Continue | `Finished ] Lwt.t
(** [receiver frame] handle input frame from Websocket. If Actor do not want to handle message anymore, return
    [`Finished]. *)

type sender = W.Frame.t -> unit
(** [sender frame] simple method to send frame to other side of Websocket. *)

(** Module signature of Actor system *)
module type S = sig
  type t

  val add_receiver : t -> receiver -> unit Lwt.t
  (** [add_receiver t receiver] add a receiver to actor system. Calculation returned from this function is blocking
      until receiver closed. *)

  val make_sender : t -> sender * (unit -> unit)
  (** [make_sender t] create a sender and delete function. Must call delete function if you do not need sender anymore. *)

  val start : t -> unit Lwt.t
  (** [start t] start the actor system. *)

  val stop : t -> unit
  (** [start t] stop the actor system [t]. When you want to wait to stop the system, use thunk returned from [start] *)
end

module type Instance = sig
  module Actor : S

  val instance : Actor.t
end
