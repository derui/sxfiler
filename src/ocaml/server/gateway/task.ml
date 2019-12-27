open Sxfiler_core
module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module Gen = Sxfiler_server_generated.Task

module Send_reply = struct
  module Type = struct
    type input = Gen.TaskSendReplyRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.TaskSendReplyResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    let input_from_pb = Gen.TaskSendReplyRequest.from_proto
    let output_to_pb = Gen.TaskSendReplyResponse.to_proto
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Send_reply.S) : S = struct
    include Type

    let handle param =
      let reply = Option.get_exn param |> T.Task_interaction.Reply.to_domain in
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

    let input_from_pb = Gen.TaskCancelRequest.from_proto
    let output_to_pb = Gen.TaskCancelResponse.to_proto
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Cancel.S) : S = struct
    include Type

    let handle param =
      let reply = T.Task_types.Task_id.to_domain param in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_ok ()
      | Error `Not_found -> Lwt.return_error Gateway_error.(Task_not_found)
  end
end
