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
    let src = Js.to_string src##.filename in
    let filename = Filename.basename src in
    let dest = N.Path.resolve [dest; filename] in
    let module Fs = N.Fs.Make(struct let instance = fs end) in
    match Fs.renameSync src dest with
    | Ok _ -> Lwt.return @@ M.finish_execute_operation (Ok ())
    | Error _ -> Lwt.return @@ M.finish_execute_operation @@ T.Operation_result.of_error ""

  let delete_file file =
    let fs = Fs.resolve () in
    let file = Js.to_string file##.filename in
    let module Fs = N.Fs.Make(struct let instance = fs end) in
    let open Minimal_monadic_caml.Result.Infix in
    match Fs.unlinkSync file with
    | Ok _ -> Lwt.return @@ M.finish_execute_operation (Ok ())
    | Error _ -> Lwt.return @@ M.finish_execute_operation @@ T.Operation_result.of_error ""

  let copy_file src dest =
    let fs = Fs.resolve () in
    let src = Js.to_string src##.filename in
    let filename = Filename.basename src in
    let dest = N.Path.resolve [dest; filename] in
    let module Fs = N.Fs.Make(struct
        let instance = fs
      end) in
    let open Lwt.Infix in
    Fs.copy_file ~src ~dest () >>= (fun ret ->
        match ret with
        | Ok _ -> Lwt.return @@ M.finish_execute_operation (Ok ())
        | Error err -> begin
            match err with
            | `FsCopyError err ->
              let error = Js.to_string err##toString in
              Lwt.return @@ M.finish_execute_operation @@ T.Operation_result.of_error error
          end
      )

  (* refresh all panes *)
  let refresh_panes left right =
    let fs = Fs.resolve () in
    let left_wait, left_waker = Lwt.wait ()
    and right_wait, right_waker = Lwt.wait () in

    let open Lwt.Infix in
    let fetch_files waker pane = refresh_pane ~fs pane >|= Lwt.wakeup waker in

    Lwt.async (fun () -> fetch_files left_waker left <&> fetch_files right_waker right);
    left_wait >>= fun left -> right_wait >>= fun right ->
    Lwt.return @@ M.finish_refresh_panes (Ok (T.Pane.to_js left, T.Pane.to_js right))

  let fetch_files ~loc ~pane ~path =
    let fs = Fs.resolve () in

    let open Lwt.Infix in
    let lwt = refresh_pane ~dir:path ~fs pane
      >>= fun pane -> Lwt.return @@ M.finish_files_in_directory (Ok (T.Pane.to_js pane, loc))
    in

    Lwt.catch (fun () -> lwt) (fun err ->
        Firebug.console##log err;
        match err with
        | File_list.Not_directory f ->
          let module M = Sxfiler_common.Message in
          Lwt.return @@ M.finish_files_in_directory @@ T.Operation_result.of_error f
        | _ -> raise Unhandled_promise
      )

  let request_files_in_directory t pane path loc =
    let pane = T.Pane.of_js pane
    and path = Js.to_string path in
    ({t with S.waiting = true}, Some (fetch_files ~pane ~path ~loc))

  let finish_files_in_directory t ret = match ret with
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
      Some ((T.Pane.to_js pane, Js.string next_dir, T.Pane_location.to_js t.S.active_pane) |> M.request_files_in_directory |> Lwt.return)
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
        Some (M.request_files_in_directory (T.Pane.to_js pane, Js.string target_dir,
                                           T.Pane_location.to_js t.S.active_pane) |> Lwt.return)
      end else
        None
    in
    (t, message)

  let move_to_another t = (S.swap_active_pane t, None)
  let request_operation t op =
    ({t with S.operation = {S.Operation.confirming = true; executing = false;next = Some op}}, None)

  let confirm_operation t confirmed =
    if confirmed then begin
      match t.S.operation.S.Operation.next with
      | None -> ({t with S.operation = {t.S.operation with S.Operation.confirming = false;executing = false}}, None)
      | Some op -> ({t with S.operation = {t.S.operation with S.Operation.confirming = false}}, Some (Lwt.return @@ M.request_execute_operation op))
    end else ({t with S.operation = {S.Operation.confirming = false; executing = false;next = None}}, None)

  let execute_operation t op =
    if t.S.operation.S.Operation.executing then (t, None)
    else begin
      match op with
      | M.Operation.Copy payload -> begin
          let module R = C.Message_payload.Request_copy_file in
          let src = payload.R.src
          and dest = Js.to_string payload.R.dest_dir in
          ({t with S.operation = {t.S.operation with S.Operation.executing = true}}, Some (copy_file src dest))
        end
      | M.Operation.Delete payload -> begin
          let module R = C.Message_payload.Request_delete_file in
          let file = payload.R.file in
          ({t with S.operation = {t.S.operation with S.Operation.executing = true}},
           Some (delete_file file))
        end
      | M.Operation.Move payload -> begin
          let module R = C.Message_payload.Request_move_file in
          let src = payload.R.src
          and dest = Js.to_string payload.R.dest_dir in
          ({t with S.operation = {t.S.operation with S.Operation.executing = true}}, Some (move_file src dest))
        end
    end

  let request_refresh_panes t = (t, Some (refresh_panes t.S.left_pane t.S.right_pane))
  let finish_refresh_panes t = function
    | Ok (left_pane, right_pane) ->
      let left_pane = T.Pane.of_js left_pane
      and right_pane = T.Pane.of_js right_pane in
      ({t with S.left_pane; right_pane}, None)
    | Error err -> ({t with S.operation_log = T.Operation_log.add_entry t.S.operation_log ~entry:err.T.Operation_result.message}, None)

  let finish_operation t ret =
    ({t with S.operation = {t.S.operation with S.Operation.executing = false; next = None}},
     Some (Lwt.return M.request_refresh_panes))

  let react t = function
    | M.Request_files_in_directory (pane, path, loc) -> request_files_in_directory t pane path loc
    | M.Finish_files_in_directory ret -> finish_files_in_directory t ret
    | M.Request_refresh_panes -> request_refresh_panes t
    | M.Finish_refresh_panes payload -> finish_refresh_panes t payload
    | M.Select_next_item v -> move_cursor t @@ abs v
    | M.Select_prev_item v -> move_cursor t (-1 * abs v)
    | M.Leave_directory -> leave_directory t
    | M.Enter_directory -> enter_directory t
    | M.Request_quit_application -> ({t with S.terminated = true}, None)
    | M.Move_to_another -> move_to_another t
    | M.Request_operation op -> request_operation t op
    | M.Confirm_operation confirmed -> confirm_operation t confirmed
    | M.Request_execute_operation op -> execute_operation t op
    | M.Finish_execute_operation ret -> finish_operation t ret
end
