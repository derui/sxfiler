open Abbrev

include module type of Keymap_intf

val add_key_binding :
  [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ] Add_key_binding.work_flow
(** implementation to add binding to key map *)

val remove_key_binding :
  [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ] Remove_key_binding.work_flow
(** implementation to remove binding from key map *)

val reload : [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ] Reload.work_flow
(** implementation of workflow to reload whole key map *)
