(** This module defines translator for {Configuration} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module T = Sxfiler_domain

module Sort_type = struct
  type t = T.Types.Sort_type.t

  let of_yojson = function
    | `Int v -> begin match v with
        | 1 -> Ok T.Types.Sort_type.Date
        | 2 -> Ok T.Types.Sort_type.Name
        | 3 -> Ok T.Types.Sort_type.Size
        | _ -> Error "Unknown sort_type"
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (match t with
      | T.Types.Sort_type.Date -> 1
      | T.Types.Sort_type.Name -> 2
      | T.Types.Sort_type.Size -> 3)

  let of_domain = Fun.ident
  let to_domain = Fun.ident
end

type t = {
  sort_order: Sort_type.t [@key "sortOrder"];
} [@@deriving yojson]

let of_domain t = {
  sort_order = Sort_type.of_domain t.T.Configuration.sort_order;
}

let to_domain t = {
  T.Configuration.sort_order = Sort_type.to_domain t.sort_order;
}
