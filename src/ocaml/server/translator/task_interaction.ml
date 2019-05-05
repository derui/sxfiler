module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction

(** level of notification. *)
module Interaction = struct
  type typ =
    | Yes_no [@name "yes-no"]
    | String [@name "string"]
    | Int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain = function T.Yes_no _ -> Yes_no | String _ -> String | Int _ -> Int
  let to_domain _ = failwith "Can not convert to domain"
end

type t =
  { id : string
  ; task_id : string [@key "taskId"]
  ; filter_interaction : Interaction.typ list [@key "filterInteraction"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  let filter_interaction =
    [T.Yes_no false; T.String ""; T.Int 0]
    |> List.filter t.D.filter_interaction
    |> List.map Interaction.of_domain
  in
  {id = Uuidm.to_string t.D.id; task_id = Uuidm.to_string t.task_id; filter_interaction}

let to_domain _ = failwith "Can not convert to domain"
