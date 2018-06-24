include Sxfiler_types.Snapshot_history

let to_js : t -> Yojson.json = fun t ->
  `Assoc [
    ("records", `List (List.map Snapshot_record.to_js t.records));
    ("maxRecords", `Int t.max_records);
  ]
