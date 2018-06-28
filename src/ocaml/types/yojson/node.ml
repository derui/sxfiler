(** Node is a item in a file tree. *)
include Sxfiler_types.Node

let to_yojson : t -> Yojson.Safe.json = fun t ->
  `Assoc [
    ("fullPath", `String t.full_path);
    ("stat", File_stat.to_yojson t.stat);
    ("parentDirectory", `String t.parent_directory);
    ("linkPath", match t.link_path with
      | None -> `Null
      | Some v -> `String v);
  ]
