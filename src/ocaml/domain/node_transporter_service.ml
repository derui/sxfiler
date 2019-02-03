(** this module defines interface to transport node between filers.
    No implementation that is default provides from this, should implement yourself.
*)
open Sxfiler_core

module type S = sig
  val transport : nodes:Node.t list -> corrections:Types.corrections -> _to:Filer.t -> unit Lwt.t
  (** [transport ~nodes ~corrections ~_to] do move [nodes] to the location [_to].
      When valid [corrections] gives this function, apply it to each node before transport.
  *)
end
