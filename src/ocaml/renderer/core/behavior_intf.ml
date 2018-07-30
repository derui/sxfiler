
(** The signature of behavior. *)
module type S = sig
  type t

  (** type of parameter for execute function *)
  type param

  type config

  (** [create config param] gets a new instance of behavior *)
  val create: config -> param -> t

  (** [execute t dispatcher param] do behavior with [param]  *)
  val execute: t -> (module Dispatcher.Instance) -> unit Lwt.t
end

module type Instance = sig
  module Behavior : S
  val this: Behavior.t
end
