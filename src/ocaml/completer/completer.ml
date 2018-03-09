type match_type =
  | Forward_exact_match
  | Partial_match

let make_regexp_matcher input =
  let input' = Js.string @@ input in
  let regexp = new%js Js.regExp input' in
  fun s ->
    let s' = Js.string s in
    Js.to_bool @@ regexp##test s'

let make_forward_exact_matcher input = make_regexp_matcher ("^" ^ input)
let make_partial_matcher = make_regexp_matcher

let make_matcher ~input = function
  | Forward_exact_match -> make_forward_exact_matcher input
  | Partial_match -> make_partial_matcher input

let complete (type v) ~input ~match_type ~candidates
    ~(stringify:(module Candidates_intf.Type with type t = v)) =
  let module S = (val stringify : Candidates_intf.Type with type t = v) in
  let matcher = make_matcher ~input match_type in
  List.filter (fun s -> matcher @@ S.to_string s) candidates
