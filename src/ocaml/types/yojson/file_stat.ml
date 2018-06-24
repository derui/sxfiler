include Sxfiler_types.File_stat

(** [to_js t] converts OCaml type to Yojson representation.
    Can not use ppx_deriving_yojson because some types can not convert as types wanted.
*)
let to_js : t -> Yojson.json = fun t ->
  `Assoc [
    ("mode", `String (Int32.to_string t.mode));
    ("uid", `Int t.uid);
    ("gid", `Int t.gid);
    ("atime", `String (Int64.to_string t.atime));
    ("ctime", `String (Int64.to_string t.ctime));
    ("mtime", `String (Int64.to_string t.mtime));
    ("size", `String (Int64.to_string t.size));
    ("isDirectory", `Bool t.is_directory);
    ("isFile", `Bool t.is_file);
    ("isSymlink", `Bool t.is_symlink);
  ]
