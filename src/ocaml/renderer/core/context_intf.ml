(** Context should be execute behavior and manage store group.  *)

module type S = sig
  (** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
  type t

  (** [get_store t ~tag] is helper function to get store directly.  *)
  val get_store : t -> tag:('a, 'b) Tag.def -> 'a

  val subscribe : t -> Store_group.event -> unit

  (** [execute instance param] execute behavior [instance] with [param]. *)
  val execute:
    t ->
    (module Behavior_intf.S with type param = 'p and type result = 'r) ->
    'p ->
    'r
end

(** Instance of context. *)
module type Instance = sig
  module Context : S
  val instance : Context.t
end
