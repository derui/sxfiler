(** Interface for filer service *)
open Abbrevs

module type S = sig
  (** [make param] calls the service to make filer with [param] .
      All exceptions raised from this are below. You can catch exceptions via {!Lwt.catch}

      @raise
  *)
  val make: E.Filer.Make.params -> (E.Filer.Make.result, [`Already_exists]) result Lwt.t

  val get: E.Filer.Get.params -> (E.Filer.Get.result, [`Not_found]) result Lwt.t
end
