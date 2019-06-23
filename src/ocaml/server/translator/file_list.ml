open Sxfiler_core
module D = Sxfiler_domain.File_list

type t =
  { location : string
  ; items : File_item.t list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain (t : D.t) =
  {location = Path.to_string t.location; items = List.map File_item.of_domain t.items}

let to_domain (t : t) =
  {D.location = Path.of_string t.location; items = List.map File_item.to_domain t.items}
