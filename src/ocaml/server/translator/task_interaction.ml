module Ty = Sxfiler_domain.Task_types
module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction

module Reply = struct
  type typ =
    | Overwrite of bool [@name "overwrite"]
    | Rename of {new_name : string [@key "newName"]} [@name "rename"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  type t =
    { reply : typ [@key "reply"]
    ; task_id : string [@key "taskId"] }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain {D.Reply.task_id; reply} =
    { task_id = Ty.show_id task_id
    ; reply =
        ( match reply with
        | D.Reply.Overwrite b -> Overwrite b
        | Rename new_name -> Rename {new_name} ) }

  let to_domain {task_id; reply} =
    let open Sxfiler_core in
    { D.Reply.task_id = Uuidm.of_string task_id |> Option.get_exn
    ; reply =
        ( match reply with
        | Overwrite b -> D.Reply.Overwrite b
        | Rename {new_name} -> Rename new_name ) }
end

module Suggestion = struct
  type typ =
    | Overwrite [@name "overwrite"]
    | Rename [@name "rename"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  type t =
    { suggestions : typ list [@key "suggestions"]
    ; node_name : string [@key "nodeName"]
    ; task_id : string [@key "taskId"] }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain {D.Suggestion.task_id; suggestions; node_name} =
    { task_id = Ty.show_id task_id
    ; node_name
    ; suggestions =
        List.map (function D.Suggestion.Overwrite -> Overwrite | Rename -> Rename) suggestions }

  let to_domain {task_id; suggestions; node_name} =
    let open Sxfiler_core in
    { D.Suggestion.task_id = Uuidm.of_string task_id |> Option.get_exn
    ; node_name
    ; suggestions =
        List.map (function Overwrite -> D.Suggestion.Overwrite | Rename -> Rename) suggestions }
end
