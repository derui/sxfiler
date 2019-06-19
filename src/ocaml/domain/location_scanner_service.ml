(** this module defines interface to transport node between filers. No implementation that is
    default provides from this, should implement yourself. *)
open Sxfiler_core

type location = Path.t

module type S = sig
  val scan : location -> File_tree.t Lwt.t
  (** [scan path] returns new [File_tree.t] scanned from location [path]. *)
end
