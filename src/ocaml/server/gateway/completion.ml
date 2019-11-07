module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup = struct
  module Type = struct
    type input = { source : T.Collection.t }
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = unit [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Setup.S) : S = struct
    include Type

    let handle param =
      let source = T.Collection.to_domain param.source in
      match%lwt U.execute { source } with
      | Ok () -> Lwt.return_ok ()
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Read = struct
  module Type = struct
    type input = { input : string } [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = T.Candidates.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Read.S) : S = struct
    include Type

    let handle param =
      let%lwt result = Usecase.execute { input = param.input } in
      match result with
      | Ok v -> Lwt.return_ok @@ T.Candidates.of_domain v
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end
