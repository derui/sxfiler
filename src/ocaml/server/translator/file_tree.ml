open Sxfiler_core
module D = Sxfiler_domain.File_tree

type t =
  { location : string
  ; nodes : Node.t list }
[@@deriving show, yojson]

let of_domain (t : D.t) =
  {location = Path.to_string t.location; nodes = List.map Node.of_domain t.nodes}

let to_domain (t : t) =
  {D.location = Path.of_string t.location; nodes = List.map Node.to_domain t.nodes}
