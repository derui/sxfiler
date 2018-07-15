(** This interface defines repository interface for each domain defined in {!Sxfiler_types}. *)
open Sxfiler_types

module type Scanner = sig
  type t

  (** [make ()] makes a new instance of repository. *)
  val make: unit -> t

  (** [get t id] should return instance of {!Scanner.t} *)
  val get: t -> string -> Scanner.t

  (** [store t scanner] should persistence a instance [scanner] *)
  val store: t -> Scanner.t -> unit

  (** [on_change t f] appends [f] to subscriber in [t]. This function is mutable. *)
  val on_change: t -> (Scanner.t -> unit) -> unit
end

module type Scanner_instance = sig
  module Repo : Scanner
  val instance : Repo.t
end
