(** Define objects for JSON friendly  *)

module File_stat = struct
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
end

module Domain = struct
  module D = Sxfiler_domain.File_stat
  module F = File_stat

  let mode_of_domain (t : D.mode) =
    { F.owner =
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

  let mode_to_domain (t : F.mode) =
    { D.owner =
        { D.writable = t.owner.writable
        ; readable = t.owner.readable
        ; executable = t.owner.executable }
    ; group =
        {writable = t.owner.writable; readable = t.owner.readable; executable = t.owner.executable}
    ; others =
        {writable = t.owner.writable; readable = t.owner.readable; executable = t.owner.executable}
    }

  module Base : Core.Translator with type t = F.t and type target = D.t = struct
    type t = F.t
    type target = D.t

    let of_target (t : D.t) =
      Ok
        { F.mode = mode_of_domain t.mode
        ; uid = t.uid
        ; gid = t.gid
        ; atime = Int64.to_string t.atime
        ; ctime = Int64.to_string t.ctime
        ; mtime = Int64.to_string t.mtime
        ; size = Int64.to_string t.size
        ; is_directory = t.is_directory
        ; is_file = t.is_file
        ; is_symlink = t.is_symlink }

    let to_target (t : F.t) =
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

  include Base
end

module Json = struct
  module T : Core.Translator with type t = File_stat.t and type target = Yojson.Safe.t = struct
    type t = File_stat.t
    type target = Yojson.Safe.t

    let of_target v = match File_stat.of_yojson v with Ok v -> Ok v | Error err -> Error err
    let to_target = File_stat.to_yojson
  end

  include T
end

include File_stat
