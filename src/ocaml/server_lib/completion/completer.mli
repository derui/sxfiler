(** Completer provides simple completion interface via string. *)

(** The type of completer. *)
type t

(** the type of candidate that is used to result of completion via this module.
    Notice start and length in this type are able to apply OCaml's string. It is position and length of "byte", not "character".
*)
type 'a candidate = {
  start: int;
  length: int;
  value: 'a
}

(** Get new instance of completer *)
val make: migemo:Migemocaml.Migemo.t -> t

(** Match [input] from [candidates] with method specified by [match_type].
    This function gives module to stringify a value of type [candidates], so
    this function can handle any type of candidate.
*)
val read:
  t ->
  input:string ->
  collection:'a list ->
  stringify:(module Collection.Type with type t = 'a) ->
  'a candidate list
