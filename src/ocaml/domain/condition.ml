(* Condition defines condition to handle timing to enable key binding.*)
module Context_set = struct
  include Set.Make (struct
    type t = string

    let compare = Pervasives.compare
  end)

  let pp fmt v = Format.fprintf fmt "%s" (to_seq v |> List.of_seq |> String.concat ",")
end

type t = Context_set.t [@@deriving show, eq]

let empty = Context_set.empty

let of_list contexts =
  List.fold_left (fun set context -> Context_set.add context set) empty contexts

let to_list t = Context_set.fold (fun c l -> c :: l) t []
let enable t ~context = Context_set.add context t
let disable t ~context = Context_set.remove context t
let subset ~current ~parts = Context_set.subset parts current

module type Repository = sig
  val store : t -> unit Lwt.t
  val resolve : unit -> t Lwt.t
end
