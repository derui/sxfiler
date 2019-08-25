module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

(** The gateway for Use Case of {!Rpc.Configuration.Get} *)
module Get = struct
  module Type = struct
    type input = unit [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type output = T.Configuration.t
    [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Configuration.Get.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output -> Lwt.return_ok @@ T.Configuration.of_domain output
      | Error () -> Lwt.return_error Gateway_error.(unknown_error "unknown error")
  end
end

(** The gateway for use case of {!Rpc.Configuration.Store} *)
module Store = struct
  module Type = struct
    type input = T.Configuration.t
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type output = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Configuration.Store.S) : S = struct
    include Type

    let handle input =
      match%lwt Usecase.execute @@ T.Configuration.to_domain input with
      | Ok () -> Lwt.return_ok ()
      | Error () -> Lwt.return_error Gateway_error.(unknown_error "unknown error")
  end
end
