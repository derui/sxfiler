include Sxfiler_types.Tree_stack

let data_to_js = function
  | Snapshot v -> `List [`String "Snapshot"; Tree_snapshot.to_js v]
  | Directory_tree v -> `List [`String "Directory_tree"; Directory_tree.to_js v]

let to_js : t -> Yojson.json = fun t ->
  `List (List.map data_to_js t)
