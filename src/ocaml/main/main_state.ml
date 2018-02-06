module S = Sxfiler_common.State
module T = Sxfiler_common.Types
module M = Sxfiler_common.Message

module Make(Reaction:Message_reaction.S) : Flux_frp.Flux.S.State
  with module Thread = Lwt
   and type message = M.t
   and type t = S.t = struct

  module Thread = Lwt

  type t = S.t

  type message = M.t
  type command = message Thread.t

  let replace_pane panes pane =
    let module P = Sxfiler_common.Types.Pane in
    Array.map (fun v -> if v.P.location = pane.P.location then pane else v) panes

  let select_pane panes loc =
    let module P = Sxfiler_common.Types.Pane in
    Array.to_list panes |> List.find_opt (fun v -> v.P.location = loc)

  let equal = ( = )
  let update t = function
    | M.REQUEST_FILES_IN_DIRECTORY (pane, path) -> (
        {t with S.waiting = true},
        Some (Reaction.request_files_in_directory pane path)
      )
    | M.FINISH_FILES_IN_DIRECTORY ret -> begin
        match ret with
        | Ok pane -> (
            {t with
             S.waiting = false;
             panes = replace_pane t.S.panes pane;
            }, None)
        | Error _ -> failwith "error"
      end
    | M.SELECT_NEXT_ITEM v ->
      let module O = Sxfiler_common.Util.Option in
      let open O.Infix in
      let t = O.get ~default:t (
          select_pane t.S.panes t.S.current_pane
          >|= (fun pane ->
              let module P = T.Pane in
              let file_count = List.length pane.P.file_list - 1 in
              {pane with P.cursor_pos = min file_count (v + pane.P.cursor_pos)})
          >|= (fun pane -> {t with S.panes = replace_pane t.S.panes pane})
        ) in
      (t, None)
    | M.SELECT_PREV_ITEM v ->
      let module O = Sxfiler_common.Util.Option in
      let open O.Infix in
      let t = O.get ~default:t (
          select_pane t.S.panes t.S.current_pane
          >|= (fun pane -> {pane with T.Pane.cursor_pos = max 0 (pane.T.Pane.cursor_pos - v)})
          >|= (fun pane -> {t with S.panes = replace_pane t.S.panes pane})
        ) in
      (t, None)
    | M.LEAVE_DIRECTORY ->
      let module O = Sxfiler_common.Util.Option in
      let open O.Infix in
      let message = select_pane t.S.panes t.S.current_pane
        >>= (fun pane ->
            let module P = T.Pane in
            let next_dir = Filename.dirname pane.P.directory in
            Some ((pane.P.location, next_dir) |> M.request_files_in_directory |> Lwt.return))
      in
      (t, message)

    | M.ENTER_DIRECTORY -> begin
        let module O = Sxfiler_common.Util.Option in
        let open O.Infix in
        let message = select_pane t.S.panes t.S.current_pane
          >>= (fun pane ->
              let module P = T.Pane in
              List.nth_opt pane.P.file_list pane.P.cursor_pos
              >>= fun item ->
              if item.T.File_stat.stat##.isDirectory |> Js.to_bool then begin
                let target_dir = item.T.File_stat.filename in
                Some (M.request_files_in_directory (pane.P.location, target_dir) |> Lwt.return)
              end else
                None
            )
        in
        (t, message)
      end
    | M.REQUEST_QUIT_APPLICATION -> ({t with S.terminated = true}, None)
end
