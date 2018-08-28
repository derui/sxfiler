(** Interface for key map service *)
open Abbrevs

module type S = sig
  val get : E.Keymap.Get.params -> E.Keymap.Get.result Lwt.t
  val enable_context : E.Keymap.Enable_context.params -> E.Keymap.Enable_context.result Lwt.t
  val disable_context : E.Keymap.Disable_context.params -> E.Keymap.Disable_context.result Lwt.t
end
