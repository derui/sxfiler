include Sxfiler_types.Snapshot_history

let to_yojson : t -> Yojson.Safe.json = fun t ->
  `Assoc [
    ("records", `List (List.map Snapshot_record.to_yojson t.records));
    ("maxRecords", `Int t.max_records);
  ]
