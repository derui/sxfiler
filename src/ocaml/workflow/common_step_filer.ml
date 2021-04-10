open Abbrev
include Common_step_intf.Filer

(* implementations *)
let get () =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_filer_instance c) in
  let module I = (val instance : Instance) in
  I.get () |> S.return_lwt

let reload_left t =
  let open S.Infix in
  let D.File_window.{ file_list; history } = t.D.Filer.left_file_window in
  let* file_list = Common_step_file_list.reload file_list in
  D.File_window.make_left ~file_list ~history |> S.return

let reload_right t =
  let open S.Infix in
  let D.File_window.{ file_list; history } = t.D.Filer.right_file_window in
  let* file_list = Common_step_file_list.reload file_list in
  D.File_window.make_right ~file_list ~history |> S.return

let request_copy_interaction item =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_interaction_instance c) in
  let module I = (val instance : Common_step_interaction.Instance) in
  S.return_lwt
  @@ match%lwt D.Interaction.Filer_copy item |> I.demand_decision with
     | D.Interaction.Filer_copy_selected v -> Lwt.return_ok v
     | Canceled                            -> Lwt.return_error Canceled
     | _                                   -> Lwt.fail_with "Invalid action"

let request_move_interaction item =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_interaction_instance c) in
  let module I = (val instance : Common_step_interaction.Instance) in
  S.return_lwt
  @@ match%lwt D.Interaction.Filer_move item |> I.demand_decision with
     | D.Interaction.Filer_move_selected v -> Lwt.return_ok v
     | Canceled                            -> Lwt.return_error Canceled
     | _                                   -> Lwt.fail_with "Invalid action"

let request_delete_interaction item =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_interaction_instance c) in
  let module I = (val instance : Common_step_interaction.Instance) in
  S.return_lwt
  @@ match%lwt D.Interaction.Filer_delete item |> I.demand_decision with
     | D.Interaction.Filer_delete_selected v -> Lwt.return_ok v
     | Canceled                              -> Lwt.return_error Canceled
     | _                                     -> Lwt.fail_with "Invalid action"
