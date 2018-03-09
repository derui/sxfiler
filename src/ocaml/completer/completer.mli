(** This module provides very simple completion function. *)

type match_type = Forward_exact_match | Partial_match

val complete :
  input:string ->
  match_type:match_type ->
  candidates:'a list ->
  stringify:(module Candidates_intf.Type with type t = 'a) ->
  'a list
