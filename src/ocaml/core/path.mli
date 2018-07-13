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

(** [resolve t] resolve current(.) and parent(..) in path [t]. *)
val resolve: t -> t
