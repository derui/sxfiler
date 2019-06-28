type capability =
  { writable : bool
  ; readable : bool
  ; executable : bool }
[@@deriving eq, show]

type mode =
  { owner : capability
  ; group : capability
  ; others : capability }
[@@deriving eq, show]

let empty_mode =
  { owner = {writable = true; readable = true; executable = true}
  ; group = {writable = true; readable = true; executable = true}
  ; others = {writable = true; readable = true; executable = true} }

type t =
  { mode : mode
  ; uid : int
  ; gid : int
  ; atime : int64
  ; ctime : int64
  ; mtime : int64
  ; size : int64
  ; is_directory : bool
  ; is_file : bool
  ; is_symlink : bool }
[@@deriving eq, make, show]
(** Type of stat of file. Note: The value of *time (atime, ctime, mtime) fields has time resolution
    in term of milliseconds, not seconds. *)
