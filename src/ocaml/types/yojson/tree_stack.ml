include Sxfiler_types.Tree_stack

let data_to_yojson = function
  | Snapshot v -> `List [`String "Snapshot"; Tree_snapshot.to_yojson v]
  | Directory_tree v -> `List [`String "Directory_tree"; Directory_tree.to_yojson v]

let to_yojson : t -> Yojson.Safe.json = fun t ->
  `List (List.map data_to_yojson t)
