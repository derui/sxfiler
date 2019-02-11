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
