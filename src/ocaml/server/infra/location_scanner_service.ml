(** This module provides implementation of node repository.*)
open Sxfiler_core

let get_node parent path =
  let path = Filename.concat parent path in
  if not @@ Sys.file_exists path then None
  else
    let stat = Unix.lstat path in
    let stat = Conv.stat_to_file_stat stat in
    let module D = Sxfiler_domain in
    let path = Path.of_string path and id = Digest.string path |> Digest.to_hex in
    Some (D.Node.make ~id ~full_path:path ~stat ~link_path:None)

let scan dir =
  let path = Path.to_string dir in
  let items = Sys.readdir path |> Array.to_list in
  let%lwt nodes = Lwt_list.map_p (fun v -> Lwt.return @@ get_node path v) items in
  let%lwt nodes = Lwt.return @@ List.map Option.get_exn @@ List.filter Option.is_some nodes in
  Lwt.return nodes
