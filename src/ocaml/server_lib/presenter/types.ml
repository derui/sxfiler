(** This module defines common and simple algebric domain. *)
open Sxfiler_domain.Types

module Sort_type = struct
  include Sort_type

  let of_yojson = function
    | `Int v -> begin match v with
        | 1 -> Ok Date
        | 2 -> Ok Name
        | 3 -> Ok Size
        | _ -> Error "Unknown sort_type"
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (match t with
      | Date -> 1
      | Name -> 2
      | Size -> 3)
end

module Layout = struct
  include Layout

  let of_yojson js =
    match js with
    | `Int v -> begin match v with
        | 1 -> Ok Side_by_side
        | _ -> Error "Unknown type"
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (match t with
      | Side_by_side -> 1)
end
