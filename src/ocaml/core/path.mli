(** Path is utility module more flexibility handling path and filename based on {!Filename} module. This module allows
    to handle pathname on windows and *nix platform without changing any code base. *)

type t
(** The type of Path. *)

type error = Empty_path

type env =
  [ `Unix
  | `Win
  ]

val equal : t -> t -> bool
(** [equal v1 v2] equals between [v1] and [v2]. *)

val of_string : ?env:env -> string -> (t, error) result
(** [of_string ?env path] converts [path] to Path object. *)

val to_string : ?env:env -> t -> string
(** [to_string ?env path] get a string representation of [path] *)

val of_list : ?env:env -> string list -> (t, error) result
(** [of_string ?env paths] converts [paths] to Path object. [paths] should be only part of path. *)

val resolve : ?env:env -> (module System.S) -> t -> t
(** [resolve system t] resolve current(.) and parent(..) in path [t]. *)

val basename : t -> string
(** [basename path] gets the base name of [path]. If [path] is directory, this function returns empty string. *)

val dirname : ?env:env -> t -> string
(** [dirname ?env path] gets the directory name of [path]. *)

val dirname_as_path : t -> t
(** [dirname_as_path path] gets a new path object of dirname of [path]. *)

val join : ?env:env -> t -> string -> t
(** [join ?env path next_place] join [next_place] to rest of [path], then return new path object *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] show pretty printed [t]. *)

val show_error : error -> string
(** [show_error e] return string representation of [e] *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error fmt e] show pretty printed [e] *)
