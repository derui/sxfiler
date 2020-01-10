module Ty = Sxfiler_domain.Task_types
module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction
module Gen = Sxfiler_server_generated

module Reply = struct
  let of_domain { D.Reply.task_id; reply } =
    let reply =
      match reply with
      | D.Reply.Overwrite b -> `Overwrite b
      | D.Reply.Rename s -> `Rename { Gen.Task.TaskReply.Rename.newName = s }
    and type' =
      match reply with
      | D.Reply.Rename _ -> Gen.Task.ReplyType.Rename
      | D.Reply.Overwrite _ -> Gen.Task.ReplyType.Overwrite
    in
    { Gen.Task.TaskReply.taskId = Ty.show_id task_id; reply; type' }

  let to_domain (t : Gen.Task.TaskReply.t) =
    let open Sxfiler_core in
    {
      D.Reply.task_id = Uuidm.of_string t.taskId |> Option.get_exn;
      reply =
        ( match t.reply with
        | `Overwrite b -> D.Reply.Overwrite b
        | `Rename rename -> Rename rename.Gen.Task.TaskReply.Rename.newName
        | `not_set -> failwith "Must set some reply" );
    }
end

module Suggestion = struct
  let of_domain { D.Suggestion.task_id; suggestions; item_name } =
    {
      Gen.Task.TaskSuggestion.taskId = Ty.show_id task_id;
      itemName = item_name;
      suggestions =
        List.map
          (function D.Suggestion.Overwrite -> Gen.Task.ReplyType.Overwrite | Rename -> Rename)
          suggestions;
    }

  let to_domain { Gen.Task.TaskSuggestion.taskId; suggestions; itemName } =
    let open Sxfiler_core in
    {
      D.Suggestion.task_id = Uuidm.of_string taskId |> Option.get_exn;
      item_name = itemName;
      suggestions =
        List.map
          (function Gen.Task.ReplyType.Overwrite -> D.Suggestion.Overwrite | Rename -> Rename)
          suggestions;
    }
end
