open Abbrev
include Common_step_intf.Filer

(* implementations *)

let reload_left : reload_left =
 fun t ->
  let open S.Infix in
  let D.File_window.{ file_list; history } = t.left_file_window in
  let* file_list = Common_step_file_list.reload file_list in
  let ret =
    let%lwt file_list = file_list in
    D.File_window.make_left ~file_list ~history |> Lwt.return
  in
  S.return ret

let reload_right : reload_right =
 fun t ->
  let open S.Infix in
  let D.File_window.{ file_list; history } = t.right_file_window in
  let* file_list = Common_step_file_list.reload file_list in
  let ret =
    let%lwt file_list = file_list in
    D.File_window.make_right ~file_list ~history |> Lwt.return
  in
  S.return ret

let request_copy_interaction : request_copy_interaction =
 fun item ->
  let open S.Infix in
  let* demand_action = S.fetch ~tag:(fun c -> `Step_interaction_demand_decision c) in
  S.return
  @@ match%lwt D.Interaction.Filer_copy item |> demand_action with
     | D.Interaction.Filer_copy_selected v -> Lwt.return_ok v
     | Canceled                            -> Lwt.return_error Canceled
     | _                                   -> Lwt.fail_with "Invalid action"

let request_move_interaction : request_move_interaction =
 fun item ->
  let open S.Infix in
  let* demand_action = S.fetch ~tag:(fun c -> `Step_interaction_demand_decision c) in
  S.return
  @@ match%lwt D.Interaction.Filer_move item |> demand_action with
     | D.Interaction.Filer_move_selected v -> Lwt.return_ok v
     | Canceled                            -> Lwt.return_error Canceled
     | _                                   -> Lwt.fail_with "Invalid action"

let request_delete_interaction : request_delete_interaction =
 fun item ->
  let open S.Infix in
  let* demand_action = S.fetch ~tag:(fun c -> `Step_interaction_demand_decision c) in
  S.return
  @@ match%lwt D.Interaction.Filer_delete item |> demand_action with
     | D.Interaction.Filer_delete_selected v -> Lwt.return_ok v
     | Canceled                              -> Lwt.return_error Canceled
     | _                                     -> Lwt.fail_with "Invalid action"
