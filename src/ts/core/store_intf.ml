(** Grouping defines how to watch state in grouped state. *)
module type Grouping = sig
  type state

  val watch_state : (state -> unit) -> state -> unit
end

(** The signature of store to define store for state [state]. *)
module type S = sig
  type message

  (** The type of state to be stored in this module. *)
  type state

  (** The type of subscriber *)
  type subscriber = state -> unit

  (** The abstract type of store. *)
  type t

  val make : state -> t
  (** [make state] gets new instance of store *)

  val subscribe : t -> f:subscriber -> unit
  (** [subscribe t f] adds [f] to subscription list in [t]. [f] will call when [t] is updated. *)

  val get : t -> state
  (** [get t] returns current state of [t]. *)

  val dispatch : t -> message -> unit
  (** [dispatch t message] dispatchs message to subscriptions registered by [subscribe] with [`Dispatch]
      type.
  *)
end

(** The signature of instance of store *)
module type Instance = sig
  type message

  module Store : S with type message = message

  val instance : Store.t
end
