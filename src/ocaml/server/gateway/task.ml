module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Send_reply = struct
  module Type = struct
    type input = T.Task_interaction.Reply.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = unit [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Task.Send_reply.S) : S = struct
    include Type

    let handle param =
      let reply = T.Task_interaction.Reply.to_domain param in
      match%lwt Usecase.execute reply with
      | Ok () -> Lwt.return_ok ()
      | Error `Not_found -> Lwt.return_error Gateway_error.(Task_not_found)
  end
end
