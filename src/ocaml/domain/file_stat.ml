type capability =
  { writable : bool
  ; readable : bool
  ; executable : bool }
[@@deriving show]

type mode =
  { owner : capability
  ; group : capability
  ; others : capability }
[@@deriving show]

let empty_mode =
  { owner = {writable = true; readable = true; executable = true}
  ; group = {writable = true; readable = true; executable = true}
  ; others = {writable = true; readable = true; executable = true} }

(** Type of stat of file. Note: The value of *time (atime, ctime, mtime) fields has time value
    in term of milliseconds, not seconds.
*)
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
[@@deriving show]

let make ~mode ~uid ~gid ~atime ~ctime ~mtime ~size ~is_directory ~is_file ~is_symlink =
  {mode; uid; gid; atime; ctime; mtime; size; is_directory; is_file; is_symlink}
