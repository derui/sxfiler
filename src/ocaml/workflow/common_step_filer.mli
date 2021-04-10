open Abbrev

include module type of Common_step_intf.Filer

val get : unit -> (D.Filer.t option, [> `Step_filer_instance of (module Instance) S.Context.t ]) S.t
(** [get ()] get the current filer instance if it was initialized before *)

val reload_left :
  D.Filer.t ->
  (D.Filer.left_file_window, [> `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t ]) S.t
(** [reload_left reload t] the step to reload left side file list *)

val reload_right :
  D.Filer.t ->
  (D.Filer.right_file_window, [> `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t ]) S.t
(** [reload_right reload t] the step to reload right side file list *)

val request_copy_interaction :
  D.File_item.t ->
  ( (D.Interaction.Filer_copy_selected.t, error) result,
    [> `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t ] )
  S.t
(** The step to send request to user for copy option *)

val request_move_interaction :
  D.File_item.t ->
  ( (D.Interaction.Filer_move_selected.t, error) result,
    [> `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t ] )
  S.t
(** The step to send request to user for move option *)

val request_delete_interaction :
  D.File_item.t ->
  ( (D.Interaction.Filer_delete_selected.t, error) result,
    [> `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t ] )
  S.t

(** The step to send request to user for delete option *)
