(** this module defines application error raises from gateway. *)

type error =
  | Unknown_error of string
  | Filer_not_found
  | Filer_already_exists
  | Filer_not_directory
  | Node_not_found
  | Plan_not_found
[@@deriving variants]

exception Gateway_error of error
