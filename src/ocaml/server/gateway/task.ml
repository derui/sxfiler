open Sxfiler_core
module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module Gen = Sxfiler_server_generated.Task

module Reply_to_overwrite = struct
  module Type = struct
    type input = Gen.TaskReplyToOverwriteRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.TaskReplyToOverwriteResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Send_reply.S) : S = struct
    include Type

    let handle (param : Gen.TaskReplyToOverwriteRequest.t) =
      let reply =
        {
          D.Task_interaction.Reply.task_id = Uuidm.of_string param.taskId |> Option.get_exn;
          reply = D.Task_interaction.Reply.Overwrite param.overwrite;
        }
      in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_ok ()
      | Error `Not_found -> Lwt.return_error Gateway_error.(Task_not_found)
  end
end

module Reply_to_rename = struct
  module Type = struct
    type input = Gen.TaskReplyToRenameRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.TaskReplyToRenameResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Send_reply.S) : S = struct
    include Type

    let handle (param : Gen.TaskReplyToRenameRequest.t) =
      let reply =
        {
          D.Task_interaction.Reply.task_id = Uuidm.of_string param.taskId |> Option.get_exn;
          reply = D.Task_interaction.Reply.Rename param.newName;
        }
      in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_ok ()
      | Error `Not_found -> Lwt.return_error Gateway_error.(Task_not_found)
  end
end

module Cancel = struct
  module Type = struct
    type input = Gen.TaskCancelRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.TaskCancelResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Cancel.S) : S = struct
    include Type

    let handle param =
      let reply = T.Task_types.Task_id.to_domain param.Gen.TaskCancelRequest.taskId in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_ok ()
      | Error `Not_found -> Lwt.return_error Gateway_error.(Task_not_found)
  end
end
