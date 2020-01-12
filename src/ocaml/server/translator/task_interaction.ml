module Ty = Sxfiler_domain.Task_types
module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction
module Gen = Sxfiler_server_generated

module Suggestion = struct
  type t = {
    suggestions : int list;
    itemName : string;
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain { D.Suggestion.task_id; suggestions; item_name } =
    {
      taskId = Ty.show_id task_id;
      itemName = item_name;
      suggestions =
        List.map
          (function D.Suggestion.Overwrite -> Gen.Task.ReplyType.Overwrite | Rename -> Rename)
          suggestions
        |> List.map Gen.Task.ReplyType.to_int;
    }

  let to_domain { taskId; suggestions; itemName } =
    let suggestions =
      List.map Gen.Task.ReplyType.from_int suggestions
      |> List.filter (function Ok _ -> true | Error _ -> false)
      |> List.map (function Ok v -> v | Error _ -> failwith "Invalid branch")
      |> List.map (function
           | Gen.Task.ReplyType.Overwrite -> D.Suggestion.Overwrite
           | Rename -> Rename)
    in
    let open Sxfiler_core in
    {
      D.Suggestion.task_id = Uuidm.of_string taskId |> Option.get_exn;
      item_name = itemName;
      suggestions;
    }
end
