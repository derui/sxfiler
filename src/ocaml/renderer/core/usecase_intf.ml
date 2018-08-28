(** The signature of behavior. *)
module type S = sig
  type t

  (** type of parameter for execute function *)
  type param

  val create : param -> t
  (** [create config param] gets a new instance of behavior *)

  val execute : t -> (module Dispatcher.Instance) -> unit Lwt.t
  (** [execute t dispatcher param] do behavior with [param]  *)
end

module type Instance = sig
  module Usecase : S

  val this : Usecase.t
end
