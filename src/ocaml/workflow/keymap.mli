open Abbrev

include module type of Keymap_intf

val add_key_binding :
  Add_key_binding.input ->
  ((event list, error) result, [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ]) S.t
(** implementation to add binding to key map *)

val remove_key_binding :
  Remove_key_binding.input ->
  ((event list, error) result, [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ]) S.t
(** implementation to remove binding from key map *)

val reload :
  Reload.input ->
  ((event list, error) result, [> `Step_keymap_instance of (module Common_step_keymap.Instance) S.Context.t ]) S.t
(** implementation of workflow to reload whole key map *)
