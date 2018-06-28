include Sxfiler_types.Snapshot_record

let to_yojson : t -> Yojson.Safe.json = fun t ->
  `Assoc [
    ("directory", `String t.directory);
    ("timestamp", `String (Int64.to_string t.timestamp));
  ]
