include Sxfiler_types.File_stat

module Js = struct
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
end

(** [to_yojson t] converts OCaml type to Yojson representation.
    Can not use ppx_deriving_yojson because some types can not convert as types wanted.
*)
let to_yojson : t -> Yojson.Safe.json = fun t -> Js.to_yojson {
    Js.mode = Int32.to_string t.mode;
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

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    mode = Int32.of_string v.Js.mode;
    uid = v.Js.uid;
    gid = v.Js.gid;
    atime = Int64.of_string v.Js.atime;
    ctime = Int64.of_string v.Js.ctime;
    mtime = Int64.of_string v.Js.mtime;
    size = Int64.of_string v.Js.size;
    is_directory = v.Js.is_directory;
    is_file = v.Js.is_file;
    is_symlink = v.Js.is_symlink;
  }
