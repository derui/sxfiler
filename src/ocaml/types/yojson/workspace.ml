include Sxfiler_types.Workspace

let to_yojson : t -> Yojson.Safe.json = fun t ->
  `Assoc [
    ("current", Tree_snapshot.to_yojson t.current);
    ("history", Snapshot_history.to_yojson t.history);
  ]
