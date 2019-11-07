module D = Sxfiler_domain

let perm_to_mode perm =
  let owner = perm land 0o700 and group = perm land 0o70 and others = perm land 0o7 in
  let int_to_cap v =
    let readable = v land 0o4 = 0o4
    and writable = v land 0o2 = 0o2
    and executable = v land 0o1 = 0o1 in
    { D.File_stat.readable; writable; executable }
  in
  {
    D.File_stat.owner = int_to_cap (owner lsr 6);
    group = int_to_cap (group lsr 3);
    others = int_to_cap others;
  }

let stat_to_file_stat stat =
  let module F = Sxfiler_domain.File_stat in
  let module T = Sxfiler_server_core.Time in
  let is_directory_kind = function Unix.S_DIR -> true | _ -> false
  and is_file_kind = function Unix.S_REG -> true | _ -> false
  and is_symlink_kind = function Unix.S_LNK -> true | _ -> false in
  F.make ~mode:(perm_to_mode stat.Unix.st_perm) ~uid:stat.Unix.st_uid ~gid:stat.Unix.st_gid
    ~atime:(T.time_to_int64 stat.Unix.st_atime)
    ~mtime:(T.time_to_int64 stat.Unix.st_mtime)
    ~ctime:(T.time_to_int64 stat.Unix.st_ctime)
    ~size:(Int64.of_int stat.Unix.st_size)
    ~is_directory:(is_directory_kind stat.Unix.st_kind)
    ~is_file:(is_file_kind stat.Unix.st_kind)
    ~is_symlink:(is_symlink_kind stat.Unix.st_kind)
