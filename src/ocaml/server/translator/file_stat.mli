(** Define objects for JSON friendly that is to use in gateway to convert two-way *)

type capability =
  { writable : bool
  ; readable : bool
  ; executable : bool }

type mode =
  { owner : capability
  ; group : capability
  ; others : capability }

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

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.File_stat.t
