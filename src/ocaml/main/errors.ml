(** This module defines errors that throw from this program.  *)

(** The errors should be able to handling in program, not exception them. *)
type errors = [
  | `Sxfiler_no_main_window
  | `Sxfiler_not_directory of string
  | `Sxfiler_unknown of exn
  | `Sxfiler_node_error of Jsoo_node.Errors.t
]

(** The main exception thrown from this program *)
exception Sxfiler_error of errors

(** Simple wrapper to create error *)
let to_error e = Sxfiler_error e
