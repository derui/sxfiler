
(** The signature of behavior. *)
module type S = sig
  type message

  type t

  (** type of parameter for execute function *)
  type param

  (** type of result for execute function *)
  type result

  (** [make constructor] return behavior instance. *)
  val make: (module Locator_intf.S) -> t

  (** [execute t dispatcher param] do behavior with [param]  *)
  val execute: t -> (module Dispatcher_intf.Instance with type message = message) -> param -> result
end
