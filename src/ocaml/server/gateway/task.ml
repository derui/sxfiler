module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Send_interaction = struct
  module Type = struct
    type params =
      { task_id : string [@key "taskId"]
      ; payload : T.Task_interaction.Interaction.t }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (Usecase : Usecase.Task.Send_interaction.S) :
    Core.Gateway with type params = Type.params and type result = Type.result = struct
    include Type

    let handle param =
      let task_id = Uuidm.of_string param.task_id |> Sxfiler_core.Option.get_exn in
      let interaction = T.Task_interaction.Interaction.to_domain param.payload in
      match%lwt Usecase.execute {Usecase.task_id; interaction} with
      | Ok () -> Lwt.return_unit
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error Task_not_found)
  end
end
