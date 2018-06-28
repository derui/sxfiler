include Sxfiler_types.Directory_tree

let to_yojson t =
  let rec convert = function
    | Nil -> `Assoc []
    | Tree (dir, children) -> `Assoc [(dir, `List (List.map convert children))]
  in
  convert t
