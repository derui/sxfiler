type id = string [@@deriving show]

(** This type defines operations in simulation. *)
module Operation = struct
  type t =
    | Move of string option
    | Copy of string option
    | Change_mode of File_stat.mode
    | Delete
  [@@deriving show]
end

(** [node_plan] is difference between current nodes and simulated it. *)
type node_plan =
  { operation : Operation.t
  ; node : Node.t }
[@@deriving show]

(** [t] is result of plan. *)
type t =
  { id : id
  ; source : Filer.t
  ; dest : Filer.t
  ; plans : node_plan list }
[@@deriving show]

let make ~id ~source ~dest ~plans = {id; source; dest; plans}

(* functions to make node plan *)
let plan_delete node = {operation = Operation.Delete; node}
let plan_move ?new_name node = {operation = Operation.Move new_name; node}
let plan_copy ?new_name node = {operation = Operation.Copy new_name; node}
let plan_change_mode ~mode node = {operation = Operation.Change_mode mode; node}
