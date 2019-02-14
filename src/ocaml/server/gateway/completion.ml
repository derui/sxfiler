module C = Sxfiler_domain.Completion
module T = Sxfiler_server_translator.Completion
module Usecase = Sxfiler_usecase.Completion

module Setup (U : Usecase.Setup.S) : sig
  type params = {source : T.Collection.t} [@@deriving of_yojson]
  type result = unit

  val handle : params -> result Lwt.t
end = struct
  type params = {source : T.Collection.t} [@@deriving of_yojson]
  type result = unit [@@deriving to_yojson]

  let handle param =
    let source = T.Collection.to_domain param.source in
    let open Lwt in
    U.execute {source} >>= fun _ -> Lwt.return_unit
end

module Read (Usecase : Usecase.Read.S) : sig
  type params = {input : string} [@@deriving of_yojson]
  type result = T.Candidates.t [@@deriving to_yojson]

  val handle : params -> result Lwt.t
end = struct
  type params = {input : string} [@@deriving of_yojson]
  type result = T.Candidates.t [@@deriving to_yojson]

  let handle param =
    let%lwt result = Usecase.execute {input = param.input} in
    match result with
    | Ok v -> Lwt.return @@ T.Candidates.of_domain v
    | Error () -> failwith "Unknown error"
end
