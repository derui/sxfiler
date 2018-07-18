(** This interface defines repository interface for each domain defined in {!Sxfiler_types}. *)
module type Scanner = sig
  type t

  (** [make ()] makes a new instance of repository. *)
  val make: unit -> t

  (** [get t id] should return instance of {!Scanner.t} *)
  val get: t -> string -> Sxfiler_types.Scanner.t

  (** [store t scanner] should persistence a instance [scanner] *)
  val store: t -> Sxfiler_types.Scanner.t -> unit

  (** [on_change t f] appends [f] to subscriber in [t]. This function is mutable. *)
  val on_change: t -> (Sxfiler_types.Scanner.t -> unit) -> unit
end

module type Scanner_instance = sig
  module Repo : Scanner
  val instance : Repo.t
end

(** Repository for keybinding. *)
module type Keybindings = sig
  type t

  (** [make ()] makes a new instance of repository. *)
  val make: unit -> t

  (** [get t ] should return instance of {!Key_map} *)
  val get: t -> Key_map.t

  (** [store t scanner] should persistence a instance [scanner] *)
  val store: t -> Key_map.t -> unit

  (** [on_change t f] appends [f] to subscriber in [t]. This function is mutable. *)
  val on_change: t -> (Key_map.t -> unit) -> unit
end

module type Keybindings_instance = sig
  module Repo : Keybindings
  val instance : Repo.t
end
