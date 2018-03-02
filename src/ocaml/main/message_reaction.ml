module C = Sxfiler_common
module T = C.Types
module E = C.Event
module M = C.Message
module N = Jsoo_node

exception Unhandled_promise

module type Fs = sig
  val resolve: unit -> N.Fs_types.t Js.t
end

module type S = sig
  module Fs : Fs

  val react: C.State.t -> M.t -> (C.State.t * M.t Lwt.t option)
end

let refresh_pane ?dir ~fs pane =
  let open Lwt.Infix in
  let directory = match dir with
    | Some dir -> N.Path.resolve [dir]
    | None -> N.Path.resolve [pane.T.Pane.directory] in
  File_list.get_file_stats ~fs directory
  >>= (fun file_list ->
      let cursor_pos = if directory <> pane.T.Pane.directory then None
        else Some pane.T.Pane.cursor_pos in
      Lwt.return @@ T.Pane.make ?cursor_pos ~file_list ~directory ()
    )

module Make(Fs:Fs) : S with module Fs = Fs = struct
  module Fs = Fs
  module S = C.State

  let move_file src dest =
    let fs = Fs.resolve () in
    let src = src.T.File_stat.filename in
    let filename = Filename.basename src in
    let dest = N.Path.resolve [dest; filename] in
    let module Fs = N.Fs.Make(struct let instance = fs end) in
    match Fs.renameSync src dest with
    | Ok _ -> Lwt.return @@ M.execute_task_response @@ T.Task_result.to_js (Ok T.Task_result.Payload_move)
    | Error _ -> Lwt.return @@ M.execute_task_response @@ T.Task_result.(to_js @@ of_error "")

  let delete_file file =
    let fs = Fs.resolve () in
    let file = file.T.File_stat.filename in
    let module Fs = N.Fs.Make(struct let instance = fs end) in
    let open Minimal_monadic_caml.Result.Infix in
    match Fs.unlinkSync file with
    | Ok _ -> Lwt.return @@ M.execute_task_response T.Task_result.(to_js (Ok Payload_delete))
    | Error _ -> Lwt.return @@ M.execute_task_response @@ T.Task_result.(to_js @@ of_error "")

  let copy_file src dest =
    let fs = Fs.resolve () in
    let src = src.T.File_stat.filename in
    let filename = Filename.basename src in
    let dest = N.Path.resolve [dest; filename] in
    let module Fs = N.Fs.Make(struct
        let instance = fs
      end) in
    let open Lwt.Infix in
    Fs.copy_file ~src ~dest () >>= (fun ret ->
        match ret with
        | Ok _ -> Lwt.return @@ M.execute_task_response T.Task_result.(to_js @@ Ok Payload_copy)
        | Error err -> begin
            match err with
            | `FsCopyError err ->
              let error = Js.to_string err##toString in
              Lwt.return @@ M.execute_task_response @@ T.Task_result.(to_js @@ of_error error)
          end
      )

  (* refresh all panes *)
  let refresh_panes left right =
    let fs = Fs.resolve () in
    let left_wait, left_waker = Lwt.wait ()
    and right_wait, right_waker = Lwt.wait () in

    let open Lwt.Infix in
    let refresh_pane' waker pane = refresh_pane ~fs pane >|= Lwt.wakeup waker in

    Lwt.async (fun () -> refresh_pane' left_waker left <&> refresh_pane' right_waker right);
    left_wait >>= fun left -> right_wait >>= fun right ->
    Lwt.return @@ M.refresh_panes_response (Ok (T.Pane.to_js left, T.Pane.to_js right))

  let update_pane ~loc ~pane ~path =
    let fs = Fs.resolve () in

    let open Lwt.Infix in
    let lwt = refresh_pane ~dir:path ~fs pane
      >>= fun pane -> Lwt.return @@ M.update_pane_response (Ok (T.Pane.to_js pane, loc))
    in

    Lwt.catch (fun () -> lwt) (fun err ->
        Firebug.console##log err;
        match err with
        | File_list.Not_directory f ->
          let module M = Sxfiler_common.Message in
          Lwt.return @@ M.update_pane_response @@ Error T.Operation_log.(Entry.to_js @@ Entry.make ~log_type:Error f)
        | _ -> raise Unhandled_promise
      )

  let update_pane_request t pane path loc =
    let pane = T.Pane.of_js pane
    and path = Js.to_string path in
    ({t with S.waiting = true}, Some (update_pane ~pane ~path ~loc))

  let update_pane_response t ret = match ret with
    | Ok (pane, loc) -> begin
        let pane = T.Pane.of_js pane in
        match T.Pane_location.of_js loc with
        | `Left -> ({t with S.waiting = false; left_pane = pane}, None)
        | `Right -> ({t with S.waiting = false; right_pane = pane}, None)
      end
    | Error _ -> failwith "error"

  let move_cursor t v =
    let module O = Sxfiler_common.Util.Option in
    let t =
      let pane = S.active_pane t in
      let module P = T.Pane in
      let file_count = List.length pane.P.file_list - 1 in
      S.update_pane t {
        pane with P.cursor_pos = max 0 @@ min file_count (v + pane.P.cursor_pos)
      } in
    (t, None)

  let leave_directory t =
    let module O = Sxfiler_common.Util.Option in
    let message =
      let pane = S.active_pane t in
      let module P = T.Pane in
      let next_dir = Filename.dirname pane.P.directory in
      Some (M.update_pane_request (T.Pane.to_js pane, Js.string next_dir, T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
    in
    (t, message)

  let enter_directory t =
    let module O = Sxfiler_common.Util.Option in
    let open Minimal_monadic_caml.Option.Infix in
    let message =
      let pane = S.active_pane t in
      let module P = T.Pane in
      List.nth_opt pane.P.file_list pane.P.cursor_pos
      >>= fun item ->
      if item.T.File_stat.stat##.isDirectory |> Js.to_bool then begin
        let target_dir = item.T.File_stat.filename in
        Some (M.update_pane_request (T.Pane.to_js pane, Js.string target_dir,
                                     T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
      end else
        None
    in
    (t, message)

  let change_active_pane t = (S.swap_active_pane t, None)
  let request_task t task =
    let dialog_type = match task with
      | `Task_copy | `Task_delete | `Task_move -> T.Dialog_confirmation task
      | `Task_rename -> T.Dialog_rename
    in
    let t = {t with S.interaction_state = S.Interaction_state.accept_task t.S.interaction_state task;
                    dialog_state = S.Dialog_state.Open dialog_type} in
    (t, None)

  let finish_user_action t action =
    match action with
    | T.User_action.Confirm task -> begin
        (t, Some (Lwt.return @@ M.execute_task_request task))
      end
    | Cancel -> ({t with S.interaction_state = S.Interaction_state.finish t.S.interaction_state}, None)

  let execute_task t = function
      | T.Task.Copy -> begin
          let active_pane = S.active_pane t
          and inactive_pane = S.inactive_pane t in
          let src = S.Pane.pointed_file active_pane in
          let dest = inactive_pane.T.Pane.directory in
          ({t with S.interaction_state = S.Interaction_state.execute t.S.interaction_state},
           Some (copy_file src dest))
        end
      | T.Task.Delete -> begin
          let active_pane = S.active_pane t in
          let file = S.Pane.pointed_file active_pane in
          ({t with S.interaction_state = S.Interaction_state.execute t.S.interaction_state},
           Some (delete_file file))
        end
      | T.Task.Move -> begin
          let active_pane = S.active_pane t
          and inactive_pane = S.inactive_pane t in
          let src = S.Pane.pointed_file active_pane in
          let dest = inactive_pane.T.Pane.directory in
          ({t with S.interaction_state = S.Interaction_state.execute t.S.interaction_state},
           Some (move_file src dest))
        end
      | T.Task.Rename _ -> failwith "not implement"

  let request_refresh_panes t = (t, Some (refresh_panes t.S.left_pane t.S.right_pane))
  let finish_refresh_panes t = function
    | Ok (left_pane, right_pane) -> begin
        let left_pane = T.Pane.of_js left_pane
        and right_pane = T.Pane.of_js right_pane in
        ({t with S.left_pane; right_pane}, None)
      end
    | Error entry ->
      ({t with S.operation_log = T.Operation_log.add_entry t.S.operation_log ~entry}, None)

  let finish_task t ret =
    ({t with S.interaction_state = S.Interaction_state.finish t.S.interaction_state},
     Some (Lwt.return M.refresh_panes_request))

  let react t = function
    | M.Update_pane_request (pane, path, loc) -> update_pane_request t pane path loc
    | M.Update_pane_response ret -> update_pane_response t ret
    | M.Refresh_panes_request -> request_refresh_panes t
    | M.Refresh_panes_response payload -> finish_refresh_panes t payload
    | M.Select_next_item v -> move_cursor t @@ abs v
    | M.Select_prev_item v -> move_cursor t (-1 * abs v)
    | M.Leave_directory -> leave_directory t
    | M.Enter_directory -> enter_directory t
    | M.Quit_application -> ({t with S.terminated = true}, None)
    | M.Change_active_pane -> change_active_pane t
    | M.Request_task task -> request_task t task
    | M.Finish_user_action action -> finish_user_action t @@ T.User_action.of_js action
    | M.Execute_task_request op -> execute_task t @@ T.Task.of_js op
    | M.Execute_task_response ret -> finish_task t ret
end
