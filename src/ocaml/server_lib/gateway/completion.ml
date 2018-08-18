module C = Sxfiler_completion
module Translator = Sxfiler_server_translator.Completion
module T = Sxfiler_rpc.Types

module type Setup = sig
  type params = {
    source: T.Completion.Item.t list;
  }

  val params_of_yojson : Yojson.Safe.json -> (params, string) result

  type result = unit

  val handle : params -> result Lwt.t
end

module Setup(U:C.Usecase.Setup) = struct
  type params = {
    source: T.Completion.Item.t list;
  }

  let params_of_yojson js =
    let open Yojson.Safe.Util in
    let open Sxfiler_core.Result.Infix in
    try
      let source = js |> member "source" |> to_list in
      let source = List.fold_left (fun accum item ->
          accum >>= fun accum -> Translator.Item.of_yojson item >>= fun item ->
          Ok (item :: accum)
        ) (Ok []) source
      in
      source >>= fun source ->
      Ok {source = List.rev source}
    with Type_error (s, _) -> Error s

  type result = unit

  let handle param =
    let source = List.map Translator.Item.to_domain param.source in
    U.execute {U.source}
end

module type Read = sig
  type params = {
    input: string;
  }

  type result = T.Completion.Candidate.t list

  val params_of_yojson: Yojson.Safe.json -> (params, string) Pervasives.result
  val result_to_yojson: result -> Yojson.Safe.json

  val handle : params -> result Lwt.t
end

module Read(Usecase:C.Usecase.Read) = struct
  type params = {
    input: string;
  } [@@deriving of_yojson]

  type result = T.Completion.Candidate.t list

  let result_to_yojson t = `List (List.map Translator.Candidate.to_yojson t)

  let handle param =
    let%lwt result = Usecase.execute {Usecase.input = param.input} in
    Lwt.return @@ List.map Translator.Candidate.of_domain result

end
