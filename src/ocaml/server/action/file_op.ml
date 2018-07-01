(** This module defines functions for operations for file and directory.
    Include operation of types of {!Sxfiler_types}.
*)

let stat_to_file_stat stat =
  let module F = Sxfiler_types_yojson.File_stat in
  let time_to_int64 time =
    let unixtime = float_of_int @@ int_of_float time in
    let utc, _ = Unix.mktime @@ Unix.gmtime time
    and milliseconds = int_of_float ((time -. unixtime) *. 1000.0) in
    let utc = Int64.(mul (of_float utc) 1000L) in
    Int64.(add (of_int milliseconds) utc)
  in
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
    ~atime:(time_to_int64 stat.Unix.st_atime)
    ~mtime:(time_to_int64 stat.Unix.st_mtime)
    ~ctime:(time_to_int64 stat.Unix.st_ctime)
    ~size:(Int64.of_int stat.Unix.st_size)
    ~is_directory:(is_directory_kind stat.Unix.st_kind)
    ~is_file:(is_file_kind stat.Unix.st_kind)
    ~is_symlink:(is_symlink_kind stat.Unix.st_kind)

let get_node parent path =
  let path = Filename.concat parent path in
  let stat = Unix.lstat path in
  let stat = stat_to_file_stat stat in
  let module T = Sxfiler_types_yojson in
  T.Node.make ~full_path:path ~parent_directory:parent ~stat ~link_path:None
