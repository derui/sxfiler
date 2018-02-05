module S = Sxfiler_common.State
module M = Sxfiler_common.Message

module Make(Reaction:Message_reaction.S) : Flux_frp.Flux.S.State
  with module Thread = Lwt
   and type message = M.t
   and type t = S.t = struct

  module Thread = Lwt

  type t = S.t

  type message = M.t
  type command = message Thread.t

  let equal = ( = )
  let update t = function
    | M.REQUEST_FILES_IN_DIRECTORY path -> (t, Some (Reaction.request_files_in_directory path))
    | M.FINISH_FILES_IN_DIRECTORY (_, path, list) -> ({t with
                                                       S.current_dir = path;
                                                       current_cursor = 0;
                                                       file_list = Array.to_list list}, None)
    | M.SELECT_NEXT_ITEM v ->
      let file_count = List.length t.file_list - 1 in
      ({t with current_cursor = min file_count (v + t.current_cursor)}, None)
    | M.SELECT_PREV_ITEM v ->
      ({t with current_cursor = max 0 (t.current_cursor - v)}, None)
    | M.LEAVE_DIRECTORY ->
      let current_dir = t.S.current_dir in
      (t, Some (Filename.dirname current_dir |> M.request_files_in_directory |> Lwt.return))
    | M.ENTER_DIRECTORY -> begin
        let module T = Sxfiler_common.Types in
        match List.nth_opt t.S.file_list t.S.current_cursor with
        | None -> (t, None)
        | Some item -> begin
            if item.T.File_stat.stat##.isDirectory |> Js.to_bool then begin
              let target_dir = item.T.File_stat.filename in
              (t, Some (M.request_files_in_directory target_dir |> Lwt.return))
            end else
              (t, None)
          end
      end
    | M.REQUEST_QUIT_APPLICATION -> ({t with S.terminated = true}, None)
end
