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

module Make(Fs:Fs) : S with module Fs = Fs = struct
  module Fs = Fs
  module S = C.State

  let fetch_files pane_id path =
    let fs = Fs.resolve () in
    let absolute = N.Path.resolve [path] in

    let open Lwt.Infix in
    let lwt = File_list.get_file_stats ~fs absolute
      >>= (fun file_list ->
          let module M = Sxfiler_common.Message in
          let pane = T.Pane.make ~file_list ~directory:absolute ~id:pane_id () in
          Lwt.return @@ M.finish_files_in_directory (Ok (T.Pane.to_js pane))
        )
    in

    let lwt = Lwt.catch (fun () -> lwt) (fun err ->
        Firebug.console##log err;
        match err with
        | File_list.Not_directory f ->
          let module M = Sxfiler_common.Message in
          Lwt.return @@ M.finish_files_in_directory (Error err)
        | _ -> raise Unhandled_promise
      )
    in
    lwt

  let request_files_in_directory t pane path = ({t with S.waiting = true}, Some (fetch_files pane path))
  let finish_files_in_directory t ret = match ret with
    | Ok pane -> begin (
        let pane = T.Pane.of_js pane in
        {t with
         S.waiting = false;
         left_pane = if T.Pane.equal t.S.left_pane pane then pane else t.S.left_pane;
         right_pane = if T.Pane.equal t.S.right_pane pane then pane else t.S.right_pane;
        }, None)
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
      Some ((pane.P.id, next_dir) |> M.request_files_in_directory |> Lwt.return)
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
        Some (M.request_files_in_directory (pane.P.id, target_dir) |> Lwt.return)
      end else
        None
    in
    (t, message)

  let move_to_another t = (S.swap_active_pane t, None)
  let request_operation t op =
    ({t with S.operation = {S.Operation.confirming = true; next = Some op}}, None)

  let confirm_operation t confirmed =
    if confirmed then failwith "not implement"
    else ({t with S.operation = {S.Operation.confirming = false; next = None}}, None)

  let react t = function
    | M.Request_files_in_directory (pane, path) -> request_files_in_directory t pane path
    | M.Finish_files_in_directory ret -> finish_files_in_directory t ret
    | M.Select_next_item v -> move_cursor t @@ abs v
    | M.Select_prev_item v -> move_cursor t (-1 * abs v)
    | M.Leave_directory -> leave_directory t
    | M.Enter_directory -> enter_directory t
    | M.Request_quit_application -> ({t with S.terminated = true}, None)
    | M.Move_to_another -> move_to_another t
    | M.Request_operation op -> request_operation t op
    | M.Confirm_operation confirmed -> confirm_operation t confirmed
end
