(** This module provides very simple completion function. *)

type match_type = Forward_exact_match | Partial_match

(** Match [input] from [candidates] with method specified by [match_type].
    This function gives module to stringify a value of type [candidates], so
    this function can handle any type of candidate.
*)
val complete :
  input:string ->
  match_type:match_type ->
  candidates:'a list ->
  stringify:(module Candidates_intf.Type with type t = 'a) ->
  'a list
