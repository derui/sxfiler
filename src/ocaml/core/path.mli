(** Pathname is utility module more flexibility handling path and filename based on {!Filename} module.
    This module allows to handle pathname on windows and *nix platform do not change codebase.
*)

exception Empty_path

(** The type of Path. *)
type t

type env = [`Unix | `Win]

(** [of_string ?env path] converts [path] to Path object. *)
val of_string: ?env:env -> (module System.S) -> string -> t

(** [to_string ?env path] get a string representation of [path] *)
val to_string: ?env:env -> t -> string

(** [of_string ?env (module System) paths] converts [paths] to Path object.
    [paths] should be only part of path.
*)
val of_list: ?env:env -> (module System.S) -> string list -> t

(** [resolve t] resolve current(.) and parent(..) in path [t]. *)
val resolve: t -> t

(** [equal v1 v2] return what [v1] and [v2] are equal. *)
val equal: t -> t -> bool
