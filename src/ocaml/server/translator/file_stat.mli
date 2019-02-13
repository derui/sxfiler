(** Define objects for JSON friendly  *)

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

(** Define translator for between domain object and gateway object  *)
module Domain : Core.Translator with type t = t and type target = Sxfiler_domain.File_stat.t

(** Define translator for between JSON and gateway object  *)
module Json : Core.Translator with type t = t and type target = Yojson.Safe.t
