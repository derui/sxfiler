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
    | Overwrite of {node_name : string [@key "nodeName"]} [@name "overwrite"]
    | Rename of {node_name : string [@key "nodeName"]} [@name "rename"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  type t =
    { suggestions : typ list [@key "suggestions"]
    ; task_id : string [@key "taskId"] }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain {D.Suggestion.task_id; suggestions} =
    { task_id = Ty.show_id task_id
    ; suggestions =
        List.map
          (function
            | D.Suggestion.Overwrite {node_name} -> Overwrite {node_name}
            | Rename {node_name} -> Rename {node_name} )
          suggestions }

  let to_domain {task_id; suggestions} =
    let open Sxfiler_core in
    { D.Suggestion.task_id = Uuidm.of_string task_id |> Option.get_exn
    ; suggestions =
        List.map
          (function
            | Overwrite {node_name} -> D.Suggestion.Overwrite {node_name}
            | Rename {node_name} -> Rename {node_name} )
          suggestions }
end
