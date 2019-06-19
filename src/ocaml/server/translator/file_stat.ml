(** Define objects for JSON friendly *)

type capability =
  { writable : bool
  ; readable : bool
  ; executable : bool }
[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

type mode =
  { owner : capability
  ; group : capability
  ; others : capability }
[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

type t =
  { mode : mode
  ; uid : int
  ; gid : int
  ; atime : string
  ; ctime : string
  ; mtime : string
  ; size : string
  ; is_directory : bool [@key "isDirectory"]
  ; is_file : bool [@key "isFile"]
  ; is_symlink : bool [@key "isSymlink"] }
[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

module Domain = struct
  module D = Sxfiler_domain.File_stat

  let mode_of_domain (t : D.mode) =
    { owner =
        { writable = t.D.owner.writable
        ; readable = t.D.owner.readable
        ; executable = t.D.owner.executable }
    ; group =
        { writable = t.D.group.writable
        ; readable = t.D.group.readable
        ; executable = t.D.group.executable }
    ; others =
        { writable = t.D.others.writable
        ; readable = t.D.others.readable
        ; executable = t.D.others.executable } }

  let mode_to_domain (t : mode) =
    { D.owner =
        { D.writable = t.owner.writable
        ; readable = t.owner.readable
        ; executable = t.owner.executable }
    ; group =
        {writable = t.group.writable; readable = t.group.readable; executable = t.group.executable}
    ; others =
        { writable = t.others.writable
        ; readable = t.others.readable
        ; executable = t.others.executable } }

  let of_domain (t : D.t) =
    { mode = mode_of_domain t.mode
    ; uid = t.uid
    ; gid = t.gid
    ; atime = Int64.to_string t.atime
    ; ctime = Int64.to_string t.ctime
    ; mtime = Int64.to_string t.mtime
    ; size = Int64.to_string t.size
    ; is_directory = t.is_directory
    ; is_file = t.is_file
    ; is_symlink = t.is_symlink }

  let to_domain (t : t) =
    { D.mode = mode_to_domain t.mode
    ; uid = t.uid
    ; gid = t.gid
    ; atime = Int64.of_string t.atime
    ; ctime = Int64.of_string t.ctime
    ; mtime = Int64.of_string t.mtime
    ; size = Int64.of_string t.size
    ; is_directory = t.is_directory
    ; is_file = t.is_file
    ; is_symlink = t.is_symlink }
end

include Domain
