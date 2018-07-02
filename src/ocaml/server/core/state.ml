module T = Sxfiler_types

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
