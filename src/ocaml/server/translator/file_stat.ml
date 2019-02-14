(** Define objects for JSON friendly  *)

type capability =
  { writable : bool
  ; readable : bool
  ; executable : bool }
[@@deriving yojson]

type mode =
  { owner : capability
  ; group : capability
  ; others : capability }
[@@deriving yojson]

type t =
  { mode : mode
  ; uid : int
  ; gid : int
  ; atime : string
  ; ctime : string
  ; mtime : string
  ; size : string
  ; is_directory : bool
  ; is_file : bool
  ; is_symlink : bool }
[@@deriving yojson]

module Domain = struct
  module D = Sxfiler_domain.File_stat

  let mode_of_domain (t : D.mode) =
    { owner =
        { writable = t.D.owner.writable
        ; readable = t.D.owner.readable
        ; executable = t.D.owner.executable }
    ; group =
        { writable = t.D.owner.writable
        ; readable = t.D.owner.readable
        ; executable = t.D.owner.executable }
    ; others =
        { writable = t.D.owner.writable
        ; readable = t.D.owner.readable
        ; executable = t.D.owner.executable } }

  let mode_to_domain (t : mode) =
    { D.owner =
        { D.writable = t.owner.writable
        ; readable = t.owner.readable
        ; executable = t.owner.executable }
    ; group =
        {writable = t.owner.writable; readable = t.owner.readable; executable = t.owner.executable}
    ; others =
        {writable = t.owner.writable; readable = t.owner.readable; executable = t.owner.executable}
    }

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
