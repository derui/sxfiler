open Sxfiler_core
(** this module defines interface to transport item between filers. No implementation that is
    default provides from this, should implement yourself. *)

type location = Path.t

module type S = sig
  val scan : location -> File_list.t Lwt.t
  (** [scan path] returns new [File_list.t] scanned from location [path]. *)
end
