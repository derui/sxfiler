module D = Sxfiler_domain.File_stat
module T = Sxfiler_rpc.Types.File_stat

let to_yojson t =
  `Assoc
    [ ("mode", `String t.T.mode)
    ; ("uid", `Int t.T.uid)
    ; ("gid", `Int t.T.gid)
    ; ("atime", `String t.T.atime)
    ; ("ctime", `String t.T.ctime)
    ; ("mtime", `String t.T.mtime)
    ; ("size", `String t.T.size)
    ; ("isDirectory", `Bool t.T.is_directory)
    ; ("isFile", `Bool t.T.is_file)
    ; ("isSymlink", `Bool t.T.is_symlink) ]


let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let mode = js |> member "mode" |> to_string
    and uid = js |> member "uid" |> to_int
    and gid = js |> member "gid" |> to_int
    and atime = js |> member "atime" |> to_string
    and ctime = js |> member "ctime" |> to_string
    and mtime = js |> member "mtime" |> to_string
    and size = js |> member "size" |> to_string
    and is_directory = js |> member "isDirectory" |> to_bool
    and is_file = js |> member "isFile" |> to_bool
    and is_symlink = js |> member "isSymlink" |> to_bool in
    Ok {T.mode; uid; gid; atime; ctime; mtime; size; is_directory; is_file; is_symlink}
  with Type_error (s, _) -> Error s


(** [of_domain t] converts domain object to [t] *)
let of_domain t =
  { T.mode = Int32.to_string t.D.mode
  ; uid = t.uid
  ; gid = t.gid
  ; atime = Int64.to_string t.atime
  ; ctime = Int64.to_string t.ctime
  ; mtime = Int64.to_string t.mtime
  ; size = Int64.to_string t.size
  ; is_directory = t.is_directory
  ; is_file = t.is_file
  ; is_symlink = t.is_symlink }


(** [to_domain t] converts [t] to domain object.*)
let to_domain t =
  { D.mode = Int32.of_string t.T.mode
  ; uid = t.uid
  ; gid = t.gid
  ; atime = Int64.of_string t.atime
  ; ctime = Int64.of_string t.ctime
  ; mtime = Int64.of_string t.mtime
  ; size = Int64.of_string t.size
  ; is_directory = t.is_directory
  ; is_file = t.is_file
  ; is_symlink = t.is_symlink }
