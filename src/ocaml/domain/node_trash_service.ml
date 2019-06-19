(** this module defines interface to trash nodes in a filer. No implementation that is default
    provides from this, should implement yourself. *)

module type S = sig
  val trash : Node.t list -> unit Lwt.t
  (** [trash nodes] trash [nodes]. This function does not accept corrections for nodes. *)
end
