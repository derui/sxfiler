module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module G = Sxfiler_server_generated.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup = struct
  module Type = struct
    type input = G.SetupRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.SetupResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Setup.S) : S = struct
    include Type

    let handle (param : G.SetupRequest.t) =
      let source = T.Collection.to_domain param.source in
      match%lwt U.execute { source } with
      | Ok () -> Lwt.return_ok ()
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Read = struct
  module Type = struct
    type input = G.ReadRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.ReadResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Read.S) : S = struct
    include Type

    let handle (param : G.ReadRequest.t) =
      let%lwt result = Usecase.execute { input = param.input } in
      match result with
      | Ok v ->
          let res = { G.ReadResponse.candidates = T.Candidates.of_domain v } in
          Lwt.return_ok res
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end
