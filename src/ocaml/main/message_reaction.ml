module C = Sxfiler_common
module T = C.Types
module E = C.Event
module M = C.Message
module N = Jsoo_node

module type Fs = sig
  val resolve: unit -> N.Module_types.fs Js.t
end

module type S = sig
  module Fs : Fs

  val react: C.State.t -> M.t -> (C.State.t * M.t Lwt.t option)
end

let refresh_pane_file_list ?dir ~fs pane =
  let open Lwt.Infix in
  let directory = match dir with
    | Some dir -> N.Path.resolve [dir]
    | None -> N.Path.resolve [pane.T.Pane.directory] in
  File_list.get_file_stats ~fs directory
  >>= (function
      | Ok file_list -> begin
          let module F = T.File_stat in
          let file_list = Array.of_list file_list in
          Lwt.return @@ T.Pane.make ~file_list ~focused_item:pane.T.Pane.focused_item
            ~marked_items:pane.T.Pane.marked_items
            ~directory ()
        end
      | Error e -> Lwt.fail @@ Errors.to_error @@ `Sxfiler_node_error e)

let restore_pane_info_from_history ~history pane =
  let module PH = C.Pane_history in
  PH.restore_pane_info ~pane history

let refresh_candidates ~fs path =
  let dirname = N.Path.dirname path
  and basename = N.Path.basename path in
  let module Fs = N.Fs.Make(struct let instance = fs end) in
  let directory =
    if Fs.existsSync @@ N.Path.join [dirname;basename] then
      N.Path.join [dirname;basename]
    else dirname in
  let open Lwt.Infix in

  File_list.get_file_stats ~fs directory
  >>= (function
      | Ok file_list ->
        let file_list' = List.map T.File_stat.to_js file_list |> Array.of_list |> Js.array in
        Lwt.return @@ M.refresh_candidates_response @@ Ok (Js.string path, file_list')
      | Error e ->
        Lwt.fail @@ Errors.to_error @@ `Sxfiler_node_error e)

module Make(Fs:Fs) : S with module Fs = Fs = struct
  module Fs = Fs
  module S = C.State

  (* refresh all panes *)
  let refresh_panes (left, left_history) (right, right_history) =
    let fs = Fs.resolve () in
    let left_wait, left_waker = Lwt.wait ()
    and right_wait, right_waker = Lwt.wait () in

    let open Lwt.Infix in
    let module PH = C.Pane_history in
    let refresh_pane' waker history pane = refresh_pane_file_list ~fs pane
      >>= Lwt.wrap1 @@ restore_pane_info_from_history ~history
      >>= Lwt.wrap1 @@ Lwt.wakeup waker in

    Lwt.async (fun () -> refresh_pane' left_waker left_history left <&> refresh_pane' right_waker right_history right);
    left_wait >>= fun left -> right_wait >>= fun right ->
    Lwt.return @@ M.refresh_panes_response (Ok (T.Pane.to_js left, T.Pane.to_js right))

  let update_pane ~loc ~pane ~path ~history =
    let fs = Fs.resolve () in

    let open Lwt.Infix in
    let lwt () = refresh_pane_file_list ~dir:path ~fs pane
      >>= Lwt.wrap1 @@ restore_pane_info_from_history ~history
      >>= fun pane -> Lwt.return @@ M.update_pane_response (Ok (T.Pane.to_js pane, loc))
    in

    Lwt.catch lwt (fun err ->
        let module M = Sxfiler_common.Message in
        Lwt.return @@ M.update_pane_response @@ Error T.Operation_log.(Entry.to_js @@ Entry.make ~log_type:Error "error happend")
      )

  let update_pane_request t pane path loc =
    let module PH = C.Pane_history in
    let pane = T.Pane.of_js pane
    and path = Js.to_string path
    and loc' = T.Pane_location.of_js loc in
    let history = S.pane_history ~loc:loc' t in
    let open Lwt.Infix in
    ({t with S.waiting = true}, Some (update_pane ~pane ~history ~path ~loc))

  let update_pane_response t ret = match ret with
    | Ok (pane, loc) -> begin
        let module PH = Sxfiler_common.Pane_history in
        let module O = Sxfiler_common.Util.Option in
        let module P = T.Pane in
        let pane = T.Pane.of_js pane in
        let loc = T.Pane_location.of_js loc in
        let current_pane = S.active_pane t in
        let active_history = S.active_pane_history t in
        let history = PH.History.make ~directory:current_pane.T.Pane.directory
            ~focused_item:current_pane.T.Pane.focused_item () in
        let history = PH.add_history ~history active_history in
        let t = S.update_pane_history t ~loc:t.S.active_pane ~history in
        let t = S.update_pane t ~loc ~pane in
        ({t with S.waiting = false}, None)

      end
    | Error _ -> failwith "error"

  let select_item t direction =
    let module O = Sxfiler_common.Util.Option in
    let module P = T.Pane in
    let module F = T.File_stat in
    let t =
      let pane = S.active_pane t in
      let index = P.index_item ~id:pane.P.focused_item pane in
      let index' = match direction with
        | `Next -> min (Array.length pane.P.file_list) (succ index)
        | `Prev -> max 0 (pred index)
      in
      let focused_item = pane.P.file_list.(index').F.id in
      S.update_pane t ~loc:t.S.active_pane ~pane:{
        pane with P.focused_item
      } in
    (t, None)

  let leave_directory t =
    let module O = Sxfiler_common.Util.Option in
    let module P = T.Pane in
    let pane = S.active_pane t in
    let next_dir = Filename.dirname pane.P.directory in
    let message =
      Some (M.update_pane_request (T.Pane.to_js pane, Js.string next_dir, T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
    in
    (t, message)

  let enter_directory t =
    let module O = Sxfiler_common.Util.Option in
    let open Minimal_monadic_caml.Option in
    let open Infix in
    let message =
      let pane = S.active_pane t in
      let module P = T.Pane in
      P.find_item ~id:pane.P.focused_item  pane
      >>= fun item ->
      if item.T.File_stat.stat##.isDirectory |> Js.to_bool then begin
        let target_dir = N.Path.join [item.T.File_stat.directory;item.T.File_stat.filename] in
        Some (M.update_pane_request (T.Pane.to_js pane, Js.string target_dir,
                                     T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
      end else
        None
    in
    (t, message)

  let jump_location t path =
    let path = N.Path.resolve [path] in
    let message =
      let pane = S.active_pane t in
      let module P = T.Pane in
      Some (M.update_pane_request (T.Pane.to_js pane, Js.string path,
                                   T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
    in
    ({t with S.dialog_state = S.Dialog_state.Close}, message)

  let change_active_pane t = (S.swap_active_pane t, None)

  let open_dialog t dialog_type =
    let t' = {t with S.dialog_state = S.Dialog_state.Open dialog_type} in
    match dialog_type with
    | C.Types.Dialog_jump -> ({t' with S.completing = Some T.Comp_file}, None)
    | Dialog_history -> begin
        let module S = C.State in
        let module H = C.Pane_history in
        let history = S.active_pane_history t in
        let candidates = C.Pane_history.sorted_history history in
        ({t' with
          S.completing = Some T.Comp_history;
          history_completion_state =
            S.History_completion_state.(refresh ~candidates t.S.history_completion_state)},
         None)
      end
    | _ -> (t', None)


  let close_dialog t action =
    let t = {t with S.dialog_state = S.Dialog_state.Close} in
    match action with
    | T.User_action.Confirm task -> begin
        (t, Some (Lwt.return @@ M.execute_task_request task))
      end
    | Cancel -> ({t with S.task_state = S.Task_state.finish t.S.task_state}, None)

  let execute_task t req =
    let module Instance = struct
      let instance = Fs.resolve ()
    end in
    let module Task = (val Predefined_tasks.of_request {Predefined_tasks.fs = (module Instance)} req) in
    let executed = Task.Task.execute Task.instance t in
    ({t with S.task_state = S.Task_state.accept_task t.S.task_state req;
             dialog_state = S.Dialog_state.close},
     Some Lwt.Infix.(executed >|= T.Task_result.to_js >|= M.execute_task_response))

  let request_refresh_panes t =
    let module PH = C.Pane_history in
    let left_history = S.pane_history ~loc:T.Pane_location.left t
    and right_history = S.pane_history ~loc:T.Pane_location.right t in
    let left = t.S.left_pane
    and right = t.S.right_pane in
    (t, Some (refresh_panes (left, left_history) (right, right_history)))

  let finish_refresh_panes t = function
    | Ok (left_pane, right_pane) -> begin
        let left_pane = T.Pane.of_js left_pane
        and right_pane = T.Pane.of_js right_pane in
        ({t with S.left_pane; right_pane}, None)
      end
    | Error entry ->
      let entry = T.Operation_log.Entry.of_js entry in
      ({t with S.operation_log = T.Operation_log.add_entry t.S.operation_log ~entry}, None)

  let finish_task t ret =
    ({t with S.task_state = S.Task_state.finish t.S.task_state},
     Some (Lwt.return M.refresh_panes_request))


  let select_completion t direction =
    let module F = S.File_completion_state in
    let module H = S.History_completion_state in
    match direction with
    | `Next -> begin
        match t.S.completing with
        | None -> (t, None)
        | Some T.Comp_file -> ({t with S.file_completion_state = F.select_next t.S.file_completion_state}, None)
        | Some T.Comp_history -> ({t with S.history_completion_state = H.select_next t.S.history_completion_state}, None)
      end
    | `Prev -> begin
        match t.S.completing with
        | None -> (t, None)
        | Some T.Comp_file -> ({t with S.file_completion_state = F.select_prev t.S.file_completion_state}, None)
        | Some T.Comp_history -> ({t with S.history_completion_state = H.select_prev t.S.history_completion_state}, None)
      end

  let complete_from_history t ~match_type ~input =
    let module T = C.Pane_history.History in
    let module Stringify = struct
      type t = T.t
      let to_string v = v.T.directory
    end in
    let module H = C.Pane_history in
    let history = S.active_pane_history t in
    let candidates = Array.to_list @@ C.Pane_history.sorted_history history in
    let items = Sxfiler_completer.Completer.complete ~input ~match_type ~candidates ~stringify:(module Stringify) in
    let items = Array.of_list items in
    let state = S.History_completion_state.(complete ~input ~items t.S.history_completion_state) in
    ({t with S.completing = Some C.Types.Comp_history; history_completion_state = state}, None)

  let request_refresh_candidates t path =
    let path' = Js.to_string path in
    match t.S.completing with
    | None -> (t, None)
    | Some T.Comp_file -> (t, Some (refresh_candidates ~fs:(Fs.resolve ()) path'))
    | Some T.Comp_history ->
      let module Cmp = Sxfiler_completer in
      complete_from_history t ~match_type:Cmp.Completer.Partial_match ~input:path'

  let finish_refresh_candidates t = function
    | Ok (path, candidates) ->
      let candidates = Js.to_array candidates |> Array.map T.File_stat.of_js in
      let state = S.File_completion_state.(refresh ~candidates t.S.file_completion_state) in
      ({t with S.completing = Some T.Comp_file;
               file_completion_state = state},
       Some (Lwt.return @@ M.complete_from_candidates M.Cmp.Forward_exact_match path))
    | Error entry ->
      let entry = T.Operation_log.Entry.of_js entry in
      let state = S.File_completion_state.(refresh ~candidates:[||] t.S.file_completion_state) in
      ({t with S.operation_log = T.Operation_log.add_entry t.S.operation_log ~entry;
               completing = None;
               file_completion_state = state},
       None)

  let complete_from_candidates t ~match_type ~input =
    if input##.length = 0 then
      (t, None)
    else begin
      let input' = Js.to_string input in
      let module Stringify = struct
        type t = T.File_stat.t
        let to_string v = N.Path.join [v.T.File_stat.directory;v.T.File_stat.filename]
      end in
      let candidates = Array.to_list t.S.file_completion_state.S.File_completion_state.items in
      let items = Sxfiler_completer.Completer.complete ~input:input' ~match_type ~candidates ~stringify:(module Stringify) in
      let items = Array.of_list items in
      let state = S.File_completion_state.(complete ~input:input' ~items t.S.file_completion_state) in
      ({t with S.completing = Some T.Comp_file; file_completion_state = state}, None)
    end

  let toggle_mark t v =
    let module S = C.State in
    let pane = S.active_pane t in
    let pane = T.Pane.toggle_mark ~id:v pane in

    (S.update_pane ~pane t, None)

  let react t = function
    | M.Update_pane_request (pane, path, loc) -> update_pane_request t pane path loc
    | M.Update_pane_response ret -> update_pane_response t ret
    | M.Refresh_panes_request -> request_refresh_panes t
    | M.Refresh_panes_response payload -> finish_refresh_panes t payload
    | M.Select_next_item -> select_item t `Next
    | M.Select_prev_item -> select_item t `Prev
    | M.Leave_directory -> leave_directory t
    | M.Enter_directory -> enter_directory t
    | M.Jump_location s -> jump_location t @@ Js.to_string s
    | M.Quit_application -> ({t with S.terminated = true}, None)
    | M.Change_active_pane -> change_active_pane t
    | M.Open_dialog state -> open_dialog t state
    | M.Close_dialog action -> close_dialog t @@ T.User_action.of_js action
    | M.Execute_task_request op -> execute_task t @@ T.Task_request.of_js op
    | M.Execute_task_response ret -> finish_task t ret
    | M.Select_next_completion -> select_completion t `Next
    | M.Select_prev_completion -> select_completion t `Prev
    | M.Refresh_candidates_request s -> request_refresh_candidates t s
    | M.Refresh_candidates_response v -> finish_refresh_candidates t v
    | M.Complete_from_candidates (match_type, input) -> complete_from_candidates t ~input ~match_type
    | M.Toggle_mark v -> toggle_mark t @@ T.File_id.of_js v
end
