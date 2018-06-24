(** Type of stat of file. Note: The value of *time (atime, ctime, mtime) fields has time value
    in term of milliseconds, not seconds.
*)
type t = {
  mode: int32;
  uid: int;
  gid: int;
  atime: int64;
  ctime: int64;
  mtime: int64;
  size: int64;
  is_directory: bool;
  is_file: bool;
  is_symlink: bool;
}

let make ~mode ~uid ~gid ~atime ~ctime ~mtime ~size ~is_directory ~is_file ~is_symlink =
  {
    mode;
    uid;
    gid;
    atime;
    ctime;
    mtime;
    size;
    is_directory;
    is_file;
    is_symlink;
  }
