module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction

module Interaction_typ = struct
  type t =
    | Yes_no [@name "yes-no"]
        | String [@name "string"]
        | Int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain = function T.Interaction_typ.Yes_no -> Yes_no | String -> String | Int -> Int
  let to_domain = function Yes_no -> T.Interaction_typ.Yes_no | String -> String | Int -> Int
end

module Interaction = struct
  type t =
    | Yes_no of bool [@name "yes-no"]
          | String of string [@name "string"]
          | Int of int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain = function
    | T.Interaction.Yes_no b -> Yes_no b
    | String s -> String s
    | Int v -> Int v

  let to_domain = function
    | Yes_no b -> T.Interaction.Yes_no b
    | String s -> String s
    | Int v -> Int v
end

type t =
  { id : string
  ; task_id : string [@key "taskId"]
  ; accept_interactions : Interaction_typ.t list [@key "acceptInteractions"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  { id = Uuidm.to_string t.D.id
  ; task_id = Uuidm.to_string t.task_id
  ; accept_interactions = List.map Interaction_typ.of_domain t.accept_interactions }

let to_domain v =
  match (Uuidm.of_string v.id, Uuidm.of_string v.task_id) with
  | None, _ | _, None -> failwith "Can not convert to domain"
  | Some id, Some task_id ->
    { D.id
    ; task_id
    ; accept_interactions = List.map Interaction_typ.to_domain v.accept_interactions }
