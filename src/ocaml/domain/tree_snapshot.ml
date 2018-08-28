(** [Tree_snapshot] has snapshot of a directory in {!Directory_tree}.
    Snapshot has a absolute path of the directory and nodes in the directory.
*)

type t = {directory : string; nodes : Node.t list}

let make ~directory ~nodes = {directory; nodes}
