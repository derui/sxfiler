include Sxfiler_types.Tree_snapshot

(** [to_js t] converts {!type:t} to {!type:Yojson.json}. *)
let to_js : t -> Yojson.json = fun t ->
  `Assoc [
    ("directory", `String t.directory);
    ("nodes", `List (List.map Node.to_js t.nodes));
  ]
