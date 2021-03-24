include module type of Common_step_intf.Filer

val reload_left : reload_left
(** [reload_left reload t] the step to reload left side file list *)

val reload_right : reload_right
(** [reload_right reload t] the step to reload right side file list *)

val request_copy_interaction : request_copy_interaction
(** The step to send request to user for copy option *)

val request_move_interaction : request_move_interaction
(** The step to send request to user for move option *)

val request_delete_interaction : request_delete_interaction
(** The step to send request to user for delete option *)
