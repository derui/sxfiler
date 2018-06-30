(** {!Tree_stack} provides simple management of stack of snapshot and directory tree,
    these will be used to workspace of task.
*)

type data = Snapshot of Tree_snapshot.t
          | Directory_tree of Directory_tree.t
type t = data list

let empty = []

let push ~data t = data :: t
let pop = function
  | [] -> None
  | data :: rest -> Some (data, rest)

let pop_ignore = function
  | [] -> []
  | _ :: rest -> rest
