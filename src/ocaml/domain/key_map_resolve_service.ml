(** this module defines interface to resolve key map from the location.
    No implementation that is default provides from this, should implement yourself.
*)
open Sxfiler_core

type location = Path.t

module type S = sig
  val resolve : location -> Key_map.t Lwt.t
  (** [resolve path] returns a [Key_map.t] scanned from location [path]. *)
end
