(* Condition defines condition to handle timing to enable key binding.*)
module Context_set = struct
  include Set.Make (struct
    type t = string

    let compare = String.compare
  end)

  let pp fmt v = Format.fprintf fmt "%s" (to_seq v |> List.of_seq |> String.concat ",")
end

type t = Context_set.t [@@deriving show, eq, ord]

let empty = Context_set.empty

let of_list contexts = List.fold_left (fun set context -> Context_set.add context set) empty contexts

let to_list t = Context_set.fold (fun c l -> c :: l) t []

let enable ~context t = Context_set.add context t

let disable ~context t = Context_set.remove context t

let subset ~parts current = Context_set.subset parts current
