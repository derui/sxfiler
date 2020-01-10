open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module Gen = Sxfiler_server_generated

module Need_interaction = struct
  type t = D.Task_interaction.Suggestion.t

  let typ : t Notification_service.typ =
    {
      to_method = (fun _ -> "notification/task/needInteraction");
      to_json = Fun.(Tr.Task_interaction.Suggestion.of_domain %> Gen.Task.TaskSuggestion.to_json);
    }
end

module Finished = struct
  type t = D.Task_types.id

  let typ : t Notification_service.typ =
    {
      to_method = (fun _ -> "notification/task/finished");
      to_json = (fun v -> `String (D.Task_types.show_id v));
    }
end

module Canceled = struct
  type t = D.Task_types.id

  let typ : t Notification_service.typ =
    {
      to_method = (fun _ -> "notification/task/canceled");
      to_json = (fun v -> `String (D.Task_types.show_id v));
    }
end
