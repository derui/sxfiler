(** This module defines translator for {File_stat} module to translate from domain to
    outer model.
*)module D = Sxfiler_domain.File_stat

type t = {
  mode: string;
  uid: int;
  gid: int;
  atime: string;
  ctime: string;
  mtime: string;
  size: string;
  is_directory: bool [@key "isDirectory"];
  is_file: bool [@key "isFile"];
  is_symlink: bool [@key "isSymlink"];
} [@@deriving yojson]


(** [of_domain t] converts domain object to [t] *)
let of_domain t = {
    mode = Int32.to_string t.D.mode;
    uid = t.uid;
    gid = t.gid;
    atime = Int64.to_string t.atime;
    ctime = Int64.to_string t.ctime;
    mtime = Int64.to_string t.mtime;
    size = Int64.to_string t.size;
    is_directory = t.is_directory;
    is_file = t.is_file;
    is_symlink = t.is_symlink;
  }

(** [to_domain t] converts [t] to domain object.*)
let to_domain t = {
    D.mode = Int32.of_string t.mode;
    uid = t.uid;
    gid = t.gid;
    atime = Int64.of_string t.atime;
    ctime = Int64.of_string t.ctime;
    mtime = Int64.of_string t.mtime;
    size = Int64.of_string t.size;
    is_directory = t.is_directory;
    is_file = t.is_file;
    is_symlink = t.is_symlink;
  }
