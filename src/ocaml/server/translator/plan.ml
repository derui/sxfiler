open Sxfiler_core
module D = Sxfiler_domain.Plan

module Correction = struct
  (** {!type:t} takes method to avoid error in transportation *)
  type t =
    | Rename of string
    | Overwrite
  [@@deriving show]

  let to_yojson = function
    | Rename v -> `Assoc [("type", `String "rename"); ("payload", `String v)]
    | Overwrite -> `Assoc [("type", `String "overwrite")]

  let of_yojson js =
    let open Yojson.Safe.Util in
    try
      let typ = js |> member "type" |> to_string in
      match typ with
      | "rename" ->
        let payload = js |> member "payload" |> to_string in
        Ok (Rename payload)
      | "overwrite" -> Ok Overwrite
      | _ -> raise (Type_error (Printf.(sprintf "Unknown correction: %s" typ), js))
    with Type_error (s, _) -> Error s

  let to_domain = function Rename name -> D.Correction.Rename name | Overwrite -> Overwrite
  let of_domain = function D.Correction.Rename name -> Rename name | Overwrite -> Overwrite
end

module Prediction = struct
  type t =
    | Need_fix
    | Fix of Correction.t
    | No_problem
  [@@deriving show]

  let to_yojson = function
    | Need_fix -> `Assoc [("type", `String "need-fix")]
    | Fix v -> `Assoc [("type", `String "fix"); ("payload", Correction.to_yojson v)]
    | No_problem -> `Assoc [("type", `String "no-problem")]

  let of_yojson js =
    let open Yojson.Safe.Util in
    try
      let typ = js |> member "type" |> to_string in
      match typ with
      | "fix" ->
        let payload = js |> member "payload" in
        Result.(Correction.of_yojson payload >|= fun v -> Fix v)
      | "no-problem" -> Ok No_problem
      | "need-fix" -> Ok Need_fix
      | _ -> raise (Type_error (Printf.(sprintf "Unknown prediction: %s" typ), js))
    with Type_error (s, _) -> Error s

  let to_domain = function
    | Need_fix -> D.Prediction.Need_fix
    | Fix correction -> Fix (Correction.to_domain correction)
    | No_problem -> No_problem

  let of_domain = function
    | D.Prediction.Need_fix -> Need_fix
    | Fix correction -> Fix (Correction.of_domain correction)
    | No_problem -> No_problem
end

(** {!Target_node} defines a target node of the plan. *)
module Target_node = struct
  type t =
    { node_id : string [@key "nodeId"]
    ; prediction : Prediction.t }
  [@@deriving show, yojson]

  let to_domain t =
    {D.Target_node.node_id = t.node_id; prediction = Prediction.to_domain t.prediction}

  let of_domain t =
    {node_id = t.D.Target_node.node_id; prediction = Prediction.of_domain t.prediction}
end

(** [!t] is result of plan. *)
type t =
  { id : string
  ; target_nodes : Target_node.t list [@key "targetNodes"] }
[@@deriving show, yojson]

module Empty_executor : D.Executor = struct
  let do_plan _ = assert false
end

(* executor can not recovery from [t], so executor in recovered is dummy *)
let to_domain t =
  { D.id = t.id
  ; executor = (module Empty_executor)
  ; target_nodes = List.map Target_node.to_domain t.target_nodes }

let of_domain t = {id = t.D.id; target_nodes = List.map Target_node.of_domain t.target_nodes}
