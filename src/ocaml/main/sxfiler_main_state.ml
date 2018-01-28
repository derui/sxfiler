module S = Sxfiler_common.State
module M = Sxfiler_common.Message

type selected_item = int

module Make(Reaction:Sxfiler_message_reaction.S) : Flux_frp.Flux.S.State
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
    | M.FINISH_FILES_IN_DIRECTORY (_, _, list) -> ({t with S.file_list = Array.to_list list}, None)
    | M.SELECT_NEXT_ITEM v ->
      let file_count = List.length t.file_list in
      ({t with selected_item = max file_count (v + t.selected_item)}, None)
    | M.SELECT_PREV_ITEM v ->
      ({t with selected_item = min 0 (v - t.selected_item)}, None)
    | _ -> failwith "not implemented"
end
