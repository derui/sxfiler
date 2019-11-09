open Sxfiler_core

(** This module provides implementation of item repository.*)

module D = Sxfiler_domain

let get_item parent path =
  let path = Filename.concat parent path in
  try
    let stat = Unix.lstat path in
    let stat = Conv.stat_to_file_stat stat in
    let module D = Sxfiler_domain in
    let path = Path.of_string path and id = Digest.string path |> Digest.to_hex in
    Ok (D.File_item.make ~id ~full_path:path ~stat ~link_path:None)
  with Unix.Unix_error (e, _, _) ->
    Error (Printf.sprintf "Error from lstat for %s: %s" path Unix.(error_message e))

let scan dir =
  let path = Path.to_string dir in
  let items = Sys.readdir path |> Array.to_list in
  let%lwt items = Lwt_list.map_p (fun v -> Lwt.return @@ get_item path v) items in
  let items =
    List.fold_left
      (fun accum item ->
        match item with
        | Ok item -> item :: accum
        | Error e ->
            Logs.warn (fun m -> m "Get error: %s" e);
            accum)
      [] items
  in
  D.File_list.make ~location:dir ~items () |> Lwt.return
