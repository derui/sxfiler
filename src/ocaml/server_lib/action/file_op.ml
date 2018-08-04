(** This module defines functions for operations for file and directory.
    Include operation of domain of {!Sxfiler_domain}.
*)

let stat_to_file_stat stat =
  let module F = Sxfiler_domain_yojson.File_stat in
  let module T = Sxfiler_server_core.Time in
  let is_directory_kind = function
    | Unix.S_DIR -> true
    | _ -> false
  and is_file_kind = function
    | Unix.S_REG -> true
    | _ -> false
  and is_symlink_kind = function
    | Unix.S_LNK -> true
    | _ -> false
  in

  F.make
    ~mode:(Int32.of_int stat.Unix.st_perm)
    ~uid:(stat.Unix.st_uid)
    ~gid:(stat.Unix.st_gid)
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
    let module T = Sxfiler_domain_yojson in
    Some (T.Node.make ~full_path:path ~parent_directory:parent ~stat ~link_path:None)
