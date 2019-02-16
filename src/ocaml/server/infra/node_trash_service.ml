(** This module provides implementation of trash service.*)
open Sxfiler_core

module D = Sxfiler_domain

type location = Path.t

(* delete whole directory *)
let rec delete_dir path =
  let paths = Sys.readdir path |> Array.to_list in
  let dirs = List.filter Sys.is_directory paths
  and files = List.filter Fun.(Sys.is_directory %> not) paths in
  let%lwt () = Lwt_list.iter_p (fun path -> Sys.remove path |> Lwt.return) files in
  let%lwt () = Lwt_list.iter_p delete_dir dirs in
  Unix.rmdir path |> Lwt.return

let trash nodes =
  Lwt_list.iter_p
    (fun node ->
       let path = node.D.Node.full_path |> Path.to_string in
       match node.D.Node.stat.is_directory with
       | true -> delete_dir path
       | false -> File.remove path |> Lwt.return )
    nodes
