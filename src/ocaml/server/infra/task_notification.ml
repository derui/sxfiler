open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

module Need_interaction = struct
  type t = D.Task_interaction.Suggestion.t

  let typ : t Notification_service.typ =
    { to_method = (fun _ -> "notification/task/needInteraction")
    ; to_json = Fun.(Tr.Task_interaction.Suggestion.(of_domain %> to_json)) }
end

module Finished = struct
  type t = D.Task_types.id

  let typ : t Notification_service.typ =
    { to_method = (fun _ -> "notification/task/finished")
    ; to_json =
        (fun v ->
          let v' = Uuidm.to_string v in
          `String v' ) }
end
