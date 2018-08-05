(** Interface for scanner service *)
open Abbrevs

module type S = sig
  (** [make param] calls the service to make scanner with [param] .
      All exceptions raised from this are below. You can catch exceptions via {!Lwt.catch}

      @raise
  *)
  val make: E.Scanner.Make.params -> (E.Scanner.Make.result, [`Already_exists]) result Lwt.t

  val get: E.Scanner.Get.params -> (E.Scanner.Get.result, [`Not_found]) result Lwt.t
end
