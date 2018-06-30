(** This module defines common and simple algebric types. *)
open Sxfiler_types.Types

module Layout = struct
  include Layout

  let of_yojson js =
    match js with
    | `String str -> Ok (of_string str)
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `String (to_string t)
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  include Source_type

  let of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or = fun js ->
    match js with
    | `String str -> Ok (of_string str)
    | _ -> Error "Unknown constructor"

  let to_yojson : t -> Yojson.Safe.json = fun t -> `String (to_string t)
end

module Candidate = struct
  type js = {
    start: int;
    length: int;
    value: Yojson.Safe.json;
  } [@@deriving yojson]
end
