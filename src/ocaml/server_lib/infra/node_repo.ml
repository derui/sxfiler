(** This module provides implementation of node repository.*)
open Sxfiler_core

module D = Sxfiler_domain

let stat_to_file_stat stat =
  let module F = Sxfiler_domain.File_stat in
  let module T = Sxfiler_server_core.Time in
  let is_directory_kind = function Unix.S_DIR -> true | _ -> false
  and is_file_kind = function Unix.S_REG -> true | _ -> false
  and is_symlink_kind = function Unix.S_LNK -> true | _ -> false in
  F.make ~mode:(Int32.of_int stat.Unix.st_perm) ~uid:stat.Unix.st_uid ~gid:stat.Unix.st_gid
    ~atime:(T.time_to_int64 stat.Unix.st_atime)
    ~mtime:(T.time_to_int64 stat.Unix.st_mtime)
    ~ctime:(T.time_to_int64 stat.Unix.st_ctime)
    ~size:(Int64.of_int stat.Unix.st_size)
    ~is_directory:(is_directory_kind stat.Unix.st_kind)
    ~is_file:(is_file_kind stat.Unix.st_kind)
    ~is_symlink:(is_symlink_kind stat.Unix.st_kind)

let get_node parent path =
  let path = Filename.concat parent path in
  if not @@ Sys.file_exists path then None
  else
    let stat = Unix.lstat path in
    let stat = stat_to_file_stat stat in
    let module D = Sxfiler_domain in
    let path = Path.of_string path and id = Digest.string path |> Digest.to_hex in
    Some (D.Node.make ~id ~full_path:path ~stat ~link_path:None)

module Core : D.Node.Repository = struct
  let find_by_dir ~dir =
    let path = Path.to_string dir in
    let items = Sys.readdir path |> Array.to_list in
    let%lwt nodes = Lwt_list.map_p (fun v -> Lwt.return @@ get_node path v) items in
    let%lwt nodes = Lwt.return @@ List.map Option.get_exn @@ List.filter Option.is_some nodes in
    Lwt.return nodes
end

include Core
