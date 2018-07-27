
(** The signature of behavior. *)
module type S = sig
  type message

  type t

  (** type of parameter for execute function *)
  type param

  (** type of result for execute function *)
  type result

  (** [execute t dispatcher param] do behavior with [param]  *)
  val execute: t -> (module Dispatcher_intf.Instance with type message = message) -> param -> result
end

module type Instance = sig
  type param
  type message
  type result
  module Behavior : S with type param := param
                       and type result := result
                       and type message := message
  val instance: Behavior.t
end
