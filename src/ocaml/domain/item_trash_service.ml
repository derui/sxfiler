(** this module defines interface to trash items in a filer. No implementation that is default
    provides from this, should implement yourself. *)

module type S = sig
  val trash : File_item.t list -> unit Lwt.t
  (** [trash items] trash [items]. This function does not accept corrections for items. *)
end
