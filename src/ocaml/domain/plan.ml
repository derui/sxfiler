(** This type defines operations in simulation. *)
module Operation = struct
  type t =
    | Append
    | Delete
    | Remained
    | Conflict
  [@@deriving show]

  let to_int = function Append -> 0 | Delete -> 1 | Remained -> 2 | Conflict -> 3

  let of_int = function
    | 0 -> Some Append
    | 1 -> Some Delete
    | 2 -> Some Remained
    | 3 -> Some Conflict
    | _ -> None
end

(** [node_plan] is difference between current nodes and simulated it. *)
type node_plan =
  { operation : Operation.t
  ; node : Node.t }
[@@deriving show]

(** [t] is result of plan. *)
type t =
  { source : node_plan list
  ; dest : node_plan list }
[@@deriving show]

let make ~source ~dest = {source; dest}

(* functions to make node plan *)
let node_to_delete node = {operation = Operation.Delete; node}
let node_to_append node = {operation = Operation.Append; node}
let node_to_remain node = {operation = Operation.Remained; node}
let node_to_conflict node = {operation = Operation.Conflict; node}
