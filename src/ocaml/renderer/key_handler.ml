open Sxfiler_types

type action = Context.t -> State.t Lwt.t

(** Base signagure of key handler. *)
module type S = sig
  (** [handle action] returns action to update state or execute side effect actions in it. *)
  val handle: Key_map.action -> action
end
