module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup = struct
  module Type = struct
    type params = {source : T.Collection.t}
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (U : Usecase.Setup.S) : S = struct
    include Type

    let handle param =
      let source = T.Collection.to_domain param.source in
      match%lwt U.execute {source} with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end

module Read = struct
  module Type = struct
    type params = {input : string}
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = T.Candidates.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (Usecase : Usecase.Read.S) : S = struct
    include Type

    let handle param =
      let%lwt result = Usecase.execute {input = param.input} in
      match result with
      | Ok v -> Lwt.return @@ T.Candidates.of_domain v
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end
