(** {!Workspace} provides simple management of workspace for snapshot and history set,
    these will be used to workspace of task.
*)

type t = {
  current: Tree_snapshot.t;
  history: Snapshot_history.t;
}

let make ~current ~history = {
  current; history;
}

let replace_current t ~snapshot ~clock =
  let current = t.current in
  {
    current = snapshot;
    history = let record = Snapshot_record.of_snapshot ~snapshot:current clock in
      Snapshot_history.add_record t.history ~record;
  }
