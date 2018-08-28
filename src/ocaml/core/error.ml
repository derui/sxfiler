(** this module defines simple composable error. Use with Or_error module more useful. *)
type t = String of string | Tag of t * string

exception Error of t

let create message = String message
let tag t tag = Tag (t, tag)

let rec to_string = function
  | String t ->
    t
  | Tag (t, tag) ->
    Printf.sprintf "%s: " tag ^ to_string t


let to_exn t = Error t
