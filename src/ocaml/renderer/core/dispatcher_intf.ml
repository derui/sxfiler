(** Dispatcher defines interface to dispatch message to any other instances.  *)

module type S = sig
  type message

  (** Type for abstracted dispatcher. *)
  type t

  (** [dispatch t message] dispatch [message] to some instances subscribed with [t] *)
  val dispatch : t -> message -> unit
end

(** Instance of dispatcher. *)
module type Instance = sig
  type message
  module Dispatcher : S with type message = message
  val instance : Dispatcher.t
end
