(** Pathname is utility module more flexibility handling path and filename based on {!Filename} module.
    This module allows to handle pathname on windows and *nix platform do not change codebase.
*)

exception Empty_path

(** The type of Path. *)
type t

type env = [`Unix | `Win]

(** [of_string ?env path] converts [path] to Path object. *)
val of_string: ?env:env -> string -> t

(** [to_string ?env path] get a string representation of [path] *)
val to_string: ?env:env -> t -> string

(** [of_string ?env paths] converts [paths] to Path object.
    [paths] should be only part of path.
*)
val of_list: ?env:env -> string list -> t

(** [resolve system t] resolve current(.) and parent(..) in path [t]. *)
val resolve: ?env:env -> (module System.S) -> t -> t

(** [basename path] gets the base name of [path].
    If [path] is directory, this function returns empty string.
*)
val basename: t -> string

(** [dirname ?env path] gets the directory name of [path].*)
val dirname: ?env:env -> t -> string
