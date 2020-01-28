open Abbrev
open Sxfiler_core

type reload_left =
  Common_step_file_list.scan_location -> Common_step_file_list.reload -> D.Filer.t -> D.Filer.left_file_window Lwt.t

type reload_right =
  Common_step_file_list.scan_location -> Common_step_file_list.reload -> D.Filer.t -> D.Filer.right_file_window Lwt.t

type error = Canceled

type request_copy_interaction =
  Common_step_interaction.demand_decision -> D.File_item.t -> (D.Interaction.Filer_copy_selected.t, error) Lwt_result.t

type request_move_interaction =
  Common_step_interaction.demand_decision -> D.File_item.t -> (D.Interaction.Filer_move_selected.t, error) Lwt_result.t

type request_delete_interaction =
  Common_step_interaction.demand_decision ->
  D.File_item.t ->
  (D.Interaction.Filer_delete_selected.t, error) Lwt_result.t

type operation_error =
  | Not_exists         of Path.t
  | No_permission      of string
  | Destination_exists of Path.t
  | Unknown            of string
[@@deriving eq, show]

type operation_input = {
  source : Path.t;
  dest : Path.t;
  overwrite : bool;
}

type get = unit -> D.Filer.t option Lwt.t
(** Get filer in application *)

type copy_item = operation_input -> (unit, operation_error) Lwt_result.t
(** copy an item from source to dest. Return [Not_exists] if source or destination is not exist. When [dest] is
    directory, this function should copy [source] to destination with same name of it. *)

type move_item = operation_input -> (unit, operation_error) Lwt_result.t
(** move an item from source to dest. Return [Not_exists_source] if source is not exist. Return [Not_exists_dest] if
    [dest] is not exists. Whem [dest] is directory, this function should move [source] to destination with same name of
    it. *)

type delete_item = D.File_item.t -> (unit, operation_error) Lwt_result.t
(** delete an item that given path. *)

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
