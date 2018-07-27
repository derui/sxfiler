(** Context should be execute behavior and manage store group.  *)

module type S = sig
  type message
  type store

  (** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
  type t

  (** [get_store t] is helper function to get store directly.  *)
  val get_store : t -> store

  val subscribe : t -> (store -> unit) -> unit

  (** [execute instance param] execute behavior [instance] with [param]. *)
  val execute:
    t ->
    (module Behavior_intf.Instance with type param = 'p and type result = 'r and type message = message) ->
    'p ->
    'r
end

(** Instance of context. *)
module type Instance = sig
  type message
  type store
  module Context : S with type store = store and type message = message
  val instance : Context.t
end
