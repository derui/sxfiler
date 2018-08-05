(** This module defines translator for {Location_record} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module D =  Sxfiler_domain.Location_record

type t = {
  location: string;
  timestamp: string;
} [@@deriving yojson]

let of_domain t = {
    location = Path.to_string t.D.location;
    timestamp = Int64.to_string t.D.timestamp;
  }

let to_domain t = {
  D.location = Path.of_string (module System.Real) t.location;
  timestamp = Int64.of_string t.timestamp;
}
