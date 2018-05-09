open Sxfiler_common

type message = Message.t
type key_action = Callable_action.t

(** Type of action. *)
type action = Context.t -> State.t Lwt.t

(** Create action from callable action with context. *)
val create: key_action -> action

(** Create action from message with context *)
val create_from_message: message -> action
