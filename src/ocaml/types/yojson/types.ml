(** This module defines common and simple algebric types. *)
open Sxfiler_types.Types

module Sort_type = struct
  include Sort_type

  let of_yojson = function
    | `Int v -> begin match of_enum v with
        | None -> Error "Unknown sort_type"
        | Some v -> Ok v
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (to_enum t)
end

module Layout = struct
  include Layout

  let of_yojson js =
    match js with
    | `Int v -> begin match of_enum v with
      | None -> Error "Unknown type"
      | Some v -> Ok v
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (to_enum t)
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  include Source_type

  let of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or = fun js ->
    match js with
    | `Int v -> begin match of_enum v with
        | None -> Error "Unknown source type"
        | Some v -> Ok v
      end
    | _ -> Error "Unknown constructor"

  let to_yojson : t -> Yojson.Safe.json = fun t -> `Int (to_enum t)
end
