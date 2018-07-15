(** The signature of store to define store for state [state]. *)
module type S = sig

  (** type of subscription *)
  type event = [`Change | `Dispatch]

  type message

  (** The type of state to be stored in this module. *)
  type state

  (** The type of subscriber *)
  type subscriber = state -> unit

  (** The abstract type of store. *)
  type t

  (** [make ()] gets new instance of store *)
  val make : unit -> t

  (** [subscribe t f] adds [f] to subscription list in [t]. [f] will call when [t] is updated. *)
  val subscribe: t -> f:subscriber -> event:event -> t

  (** [get t] returns current state of [t]. *)
  val get : t -> state

  (** [dispatch t message] dispatchs message to subscriptions registered by [subscribe] with [`Dispatch]
      type.
  *)
  val dispatch: t -> message -> t
end
