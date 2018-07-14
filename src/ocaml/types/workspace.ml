(** {!Workspace} provides simple management of workspace for snapshot and history set,
    these will be used to workspace of task.
*)

type t = {
  source: Tree_snapshot.t;
  target: Tree_snapshot.t;
}

let make ~source ~target = {
  source; target;
}
