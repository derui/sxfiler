module T = Sxfiler_domain
module C = Sxfiler_completion
module Translator = Sxfiler_server_translator.Completion

module type Setup = sig
  type params = {
    source: Translator.Item.t list;
  } [@@deriving of_yojson]

  type result = unit

  val handle : params -> result Lwt.t
end

module Setup(U:C.Usecase.Setup) = struct
  type params = {
    source: Translator.Item.t list;
  } [@@deriving of_yojson]

  type result = unit

  let handle param =
    let source = List.map Translator.Item.to_domain param.source in
    U.execute {U.source}
end

module type Read = sig
  type params = {
    input: string;
  } [@@deriving of_yojson]

  type result = Translator.Candidate.t list [@@deriving to_yojson]

  val handle : params -> result Lwt.t
end

module Read(Usecase:C.Usecase.Read) = struct
  type params = {
    input: string;
  } [@@deriving of_yojson]

  type result = Translator.Candidate.t list [@@deriving to_yojson]

  let handle param =
    let%lwt result = Usecase.execute {Usecase.input = param.input} in
    Lwt.return @@ List.map Translator.Candidate.of_domain result

end
