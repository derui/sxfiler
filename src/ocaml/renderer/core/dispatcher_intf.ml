(** Dispatcher defines interface to dispatch message to any other instances.  *)

module type S = sig
  (** Type for abstracted dispatcher. *)
  type t

  type config

  val create : config -> t
  (** [create config] gets new instance of dispatcher. *)

  val dispatch : t -> Message.t -> unit
  (** [dispatch t message] dispatch [message] to some instances subscribed with [t] *)
end

(** Instance of dispatcher. *)
module type Instance = sig
  module Dispatcher : S

  val this : Dispatcher.t
end
