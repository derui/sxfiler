module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup = struct
  module type S = sig
    type params = {source : T.Collection.t} [@@deriving of_yojson]

    val handle : params -> unit Lwt.t
  end

  module Make (U : Usecase.Setup.S) : S = struct
    type params = {source : T.Collection.t} [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]

    let handle param =
      let source = T.Collection.to_domain param.source in
      match%lwt U.execute {source} with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end

module Read = struct
  module type S = sig
    type params = {input : string} [@@deriving of_yojson]
    type result = T.Candidates.t [@@deriving to_yojson]

    val handle : params -> result Lwt.t
  end

  module Make (Usecase : Usecase.Read.S) : S = struct
    type params = {input : string} [@@deriving of_yojson]
    type result = T.Candidates.t [@@deriving to_yojson]

    let handle param =
      let%lwt result = Usecase.execute {input = param.input} in
      match result with
      | Ok v -> Lwt.return @@ T.Candidates.of_domain v
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end
