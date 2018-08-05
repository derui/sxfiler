(** Interface for configuration service *)
open Abbrevs

module type S = sig
  val get: E.Configuration.Get.params -> E.Configuration.Get.result Lwt.t
end
