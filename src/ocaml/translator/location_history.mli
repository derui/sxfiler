open Sxfiler_generated

type error =
  | Invalid_path      of string
  | Invalid_timestamp of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Filer.LocationHistory.t
     and type domain := Sxfiler_domain.Location_history.t
     and type error := error
