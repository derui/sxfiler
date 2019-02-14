open Sxfiler_core
module D = Sxfiler_domain.Plan

module Correction = struct
  (** {!type:t} takes method to avoid error in transportation *)
  type t =
    | Rename of string
    | Overwrite

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
end

module Prediction = struct
  type t =
    | Need_fix
    | Fix of Correction.t
    | No_problem

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
end

(** {!Target_node} defines a target node of the plan. *)
module Target_node = struct
  type t =
    { node_id : string [@key "nodeId"]
    ; prediction : Prediction.t }
  [@@deriving yojson]
end

(** [!t] is result of plan. *)
type t =
  { id : string
  ; target_nodes : Target_node.t list [@key "targetNodes"] }
[@@deriving yojson]
