open Sxfiler_core
(** This module provides implementation of trash service.*)

module D = Sxfiler_domain

(* delete whole directory *)
let rec delete_dir path =
  let paths = Sys.readdir path |> Array.to_list in
  let dirs = List.filter Sys.is_directory paths
  and files = List.filter Fun.(Sys.is_directory %> not) paths in
  let%lwt () = Lwt_list.iter_p (fun path -> Sys.remove path |> Lwt.return) files in
  let%lwt () = Lwt_list.iter_p delete_dir dirs in
  Unix.rmdir path |> Lwt.return

let trash items =
  Lwt_list.iter_p
    (fun item ->
       let path = item.D.File_item.full_path |> Path.to_string in
       match item.D.File_item.stat.is_directory with
       | true -> delete_dir path
       | false -> File.remove path |> Lwt.return)
    items
