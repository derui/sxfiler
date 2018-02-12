open Sxfiler_common

type key_action = Key_bindable_action.t
type message = Message.t

(** Convert key_action to message. *)
val to_message: State.t -> key_action -> message
