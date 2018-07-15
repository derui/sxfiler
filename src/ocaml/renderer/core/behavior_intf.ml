
(** The signature of behavior. *)
module type S = sig
  type t

  (** type of parameter for execute function *)
  type param

  (** type of result for execute function *)
  type result

  (** [make constructor] return behavior instance. *)
  val make: (module Locator_intf.S) -> t

  (** [execute param] do behavior with [param]  *)
  val execute: t -> param -> result
end
