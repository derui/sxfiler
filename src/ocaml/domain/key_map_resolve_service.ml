open Sxfiler_core
(** this module defines interface to resolve key map from the location. No implementation that is
    default provides from this, should implement yourself. *)

type location = Path.t

module type S = sig
  val resolve : unit -> Key_map.t Lwt.t
  (** [resolve ()] returns a [Key_map.t] scanned from something. *)
end
