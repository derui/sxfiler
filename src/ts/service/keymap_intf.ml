(** Interface for key map service *)
open Abbrevs

module type S = sig
  val get : E.Keymap.Get.params -> E.Keymap.Get.result Lwt.t
  val add_context : E.Keymap.Add_context.params -> E.Keymap.Add_context.result Lwt.t
  val delete_context : E.Keymap.Delete_context.params -> E.Keymap.Delete_context.result Lwt.t
end
