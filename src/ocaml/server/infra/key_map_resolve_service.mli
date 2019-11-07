module type Location = sig
  val path : Sxfiler_core.Path.t
end

(** Implementation for {!Sxfiler_domain.Key_map_resolve_service.S} *)
module Make (L : Location) : Sxfiler_domain.Key_map_resolve_service.S
