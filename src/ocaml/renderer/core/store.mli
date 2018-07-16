(* Very simple store implementation *)

module type S = Store_intf.S

(** [Make(T)] gets the new module for module with state defined from {!Type}. *)
module Make(T:State_intf.S) : S with type state = T.t and type message = T.message

module Make_group(T:State_intf.S)(G:Store_intf.Grouping with type state = T.t) : S
  with type state = T.t and type message = T.message
