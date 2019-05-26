module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Send_reply = struct
  module Type = struct
    type params = T.Task_interaction.Reply.t
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (Usecase : Usecase.Task.Send_reply.S) :
    Core.Gateway with type params = Type.params and type result = Type.result = struct
    include Type

    let handle param =
      let reply = T.Task_interaction.Reply.to_domain param in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_unit
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error Task_not_found)
  end
end
