module T = Sxfiler_types
module Ty = Sxfiler_types_yojson

module Tree_stack_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

type t = {
  configuration: T.Configuration.t;
  tree_stack_map: T.Tree_stack.t Tree_stack_map.t;
}

let empty = {
  configuration = T.Configuration.default;
  tree_stack_map = Tree_stack_map.empty;
}

(** Application global state. Do not use directly. *)
let root_state = ref empty

(* The mutex to synchronize between threads. *)
let mutex : Lwt_mutex.t = Lwt_mutex.create ()

let get_current_state = !root_state

let with_lock f =
  let%lwt updated_state = Lwt_mutex.with_lock mutex (fun () -> f !root_state) in
  Lwt.return (root_state := updated_state)
