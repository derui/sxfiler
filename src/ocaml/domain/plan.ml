(** This type defines operations in simulation. *)
module Operation = struct
  type t =
    | Append
    | Delete
    | Remained

  let to_int = function Append -> 0 | Delete -> 1 | Remained -> 2
  let of_int = function 0 -> Some Append | 1 -> Some Delete | 2 -> Some Remained | _ -> None
end

(** [node_plan] is difference between current nodes and simulated it. *)
type node_plan =
  { operation : Operation.t
  ; node : Node.t }

(** [t] is result of plan. *)
type t =
  { workbench_id : Workbench.id
  ; source : node_plan list
  ; dest : node_plan list }

let make ~workbench_id ~source ~dest = {workbench_id; source; dest}
(* functions to make node plan *)
let node_to_delete node = {operation = Operation.Delete; node}
let node_to_append node = {operation = Operation.Append; node}
let node_to_remain node = {operation = Operation.Remained; node}
