include Sxfiler_types.Tree_snapshot

(** [to_yojson t] converts {!type:t} to {!type:Yojson.json}. *)
let to_yojson : t -> Yojson.Safe.json = fun t ->
  `Assoc [
    ("directory", `String t.directory);
    ("nodes", `List (List.map Node.to_yojson t.nodes));
  ]
