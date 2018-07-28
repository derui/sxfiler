(** Context should be execute behavior and manage store group.  *)

module type S = sig
  type config

  (** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
  type t

  (** [create config] gets a new instance of Context *)
  val create: config -> t

  (** [execute behavior] execute behavior with this context. *)
  val execute: t -> (module Behavior.Instance) -> Behavior.result

  (** [dispatcher t] returns current dispatcher of context [t] *)
  val dispatcher: t -> (module Dispatcher.Instance)
end

(** Instance of context. *)
module type Instance = sig
  module Context : S
  val this : Context.t
end
