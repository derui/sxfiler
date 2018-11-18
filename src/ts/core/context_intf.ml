(** Context should be execute usecase and manage store group.  *)

module type S = sig
  type config

  (** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
  type t

  val create : config -> t
  (** [create config] gets a new instance of Context *)

  val execute : t -> (module Usecase.Instance) -> unit Lwt.t
  (** [execute usecase] execute usecase with this context. *)

  val dispatcher : t -> (module Dispatcher.Instance)
  (** [dispatcher t] returns current dispatcher of context [t] *)
end

(** Instance of context. *)
module type Instance = sig
  module Context : S

  val this : Context.t
end
