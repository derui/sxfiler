module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module G = Sxfiler_server_generated.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup = struct
  module Type = struct
    type input = G.SetupRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.SetupResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    let input_from_pb = G.SetupRequest.from_proto
    let output_to_pb = G.SetupResponse.to_proto
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Setup.S) : S = struct
    include Type

    let handle param =
      let source = T.Collection.to_domain param in
      match%lwt U.execute { source } with
      | Ok () -> Lwt.return_ok ()
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Read = struct
  module Type = struct
    type input = G.ReadRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.ReadResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    let input_from_pb = G.ReadRequest.from_proto
    let output_to_pb = G.ReadResponse.to_proto
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Read.S) : S = struct
    include Type

    let handle param =
      let%lwt result = Usecase.execute { input = param } in
      match result with
      | Ok v -> Lwt.return_ok @@ T.Candidates.of_domain v
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end
