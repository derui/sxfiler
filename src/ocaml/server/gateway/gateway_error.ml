(** The module defines all error type in gateway. Modules implemented {!Core.Gateway} will raise exception
    defined in this module.
*)
type t =
  | Unknown_error of string
  | Filer_not_found
  | Filer_already_exists
  | Filer_not_directory
  | Node_not_found
  | Plan_not_found
  | Plan_same_filer
[@@deriving variants]

exception Gateway_error of t