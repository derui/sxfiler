(* Condition defines condition to handle timing to enable key binding.*)
type context =
  | On_file_tree
  | On_completing

module Context_set = Set.Make(struct
    type t = context
    let compare = Pervasives.compare
  end)

type t = Context_set.t

let equal = Context_set.equal

let empty = Context_set.empty
let of_list contexts = List.fold_left (fun set context -> Context_set.add context set)
    empty contexts

let to_list t = Context_set.fold (fun c l -> c :: l) t []

let enable t ~context = Context_set.add context t
let disable t ~context = Context_set.remove context t
let subset ~current ~parts = Context_set.subset parts current