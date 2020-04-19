open Sxfiler_core
module D = Sxfiler_domain

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

type copy_item = operation_input -> (unit, operation_error) Lwt_result.t

type move_item = operation_input -> (unit, operation_error) Lwt_result.t

type delete_item = D.File_item.t -> (unit, operation_error) Lwt_result.t

(* implementations *)

let reload_left : reload_left =
 fun scan_location reload t ->
  let D.File_window.{ file_list; history } = t.left_file_window in
  let%lwt file_list = reload scan_location file_list in
  D.File_window.make_left ~file_list ~history |> Lwt.return

let reload_right : reload_right =
 fun scan_location reload t ->
  let D.File_window.{ file_list; history } = t.right_file_window in
  let%lwt file_list = reload scan_location file_list in
  D.File_window.make_right ~file_list ~history |> Lwt.return

let request_copy_interaction : request_copy_interaction =
 fun demand_action item ->
  match%lwt D.Interaction.Filer_copy item |> demand_action with
  | D.Interaction.Filer_copy_selected v -> Lwt.return_ok v
  | Canceled                            -> Lwt.return_error Canceled
  | _                                   -> Lwt.fail_with "Invalid action"

let request_move_interaction : request_move_interaction =
 fun demand_action item ->
  match%lwt D.Interaction.Filer_move item |> demand_action with
  | D.Interaction.Filer_move_selected v -> Lwt.return_ok v
  | Canceled                            -> Lwt.return_error Canceled
  | _                                   -> Lwt.fail_with "Invalid action"

let request_delete_interaction : request_delete_interaction =
 fun demand_action item ->
  match%lwt D.Interaction.Filer_delete item |> demand_action with
  | D.Interaction.Filer_delete_selected v -> Lwt.return_ok v
  | Canceled                              -> Lwt.return_error Canceled
  | _                                     -> Lwt.fail_with "Invalid action"
