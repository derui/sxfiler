open Abbrev

include module type of Theme_intf

val update_theme :
  Update_theme.input ->
  (Update_theme.output, [> `Step_theme_instance of (module Common_step_theme.Instance) S.Context.t ]) S.t
(** implementation to add theme *)

val get_theme :
  Get_theme.input ->
  (Get_theme.output, [> `Step_theme_instance of (module Common_step_theme.Instance) S.Context.t ]) S.t
(** implementation to list theme *)
