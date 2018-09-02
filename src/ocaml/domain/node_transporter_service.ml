(** this module defines interface to transport node between filers.
    No implementation that is default provides from this, should implement yourself.
*)
open Sxfiler_core

module type S = sig
  type location = Path.t

  val transport : nodes:Node.t list -> _to:location -> unit Lwt.t
  (** [transport ~nodes ~_to] do move [nodes] to the location [_to]. *)
end
