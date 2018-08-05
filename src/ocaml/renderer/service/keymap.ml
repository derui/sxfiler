(** Interface for key map service *)
open Abbrevs

module type S = sig
  val get: E.Keymap.Get.params -> E.Keymap.Get.result Lwt.t
end
