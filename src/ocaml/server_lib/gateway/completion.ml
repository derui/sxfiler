module C = Sxfiler_domain.Completion
module Translator = Sxfiler_server_translator.Completion
module T = Sxfiler_rpc.Types
module Usecase = Sxfiler_usecase

module type Setup = sig
  type params = {source : T.Completion.Item.t list}

  val params_of_yojson : Yojson.Safe.json -> (params, string) result

  type result = unit

  val handle : params -> result Lwt.t
end

module Setup (U : Usecase.Completion.Setup.S) = struct
  type params = {source : T.Completion.Item.t list}

  let params_of_yojson js =
    let open Yojson.Safe.Util in
    let open Sxfiler_core.Result in
    try
      let source = js |> member "source" |> to_list in
      let source =
        List.fold_left
          (fun accum item ->
             accum >>= fun accum -> Translator.Item.of_yojson item >>= fun item -> Ok (item :: accum)
          )
          (Ok []) source
      in
      source >>= fun source -> Ok {source = List.rev source}
    with Type_error (s, _) -> Error s

  type result = unit

  let handle param =
    let source = List.map Translator.Item.to_domain param.source in
    let open Lwt in
    U.execute {source} >>= fun _ -> Lwt.return_unit
end

module type Read = sig
  type params = {input : string}
  type result = T.Completion.Candidate.t list

  val params_of_yojson : Yojson.Safe.json -> (params, string) Pervasives.result
  val result_to_yojson : result -> Yojson.Safe.json
  val handle : params -> result Lwt.t
end

module Read (Usecase : Usecase.Completion.Read.S) = struct
  type params = {input : string} [@@deriving of_yojson]
  type result = T.Completion.Candidate.t list

  let result_to_yojson t = `List (List.map Translator.Candidate.to_yojson t)

  let handle param =
    let%lwt result = Usecase.execute {input = param.input} in
    match result with
    | Ok v -> Lwt.return @@ List.map Translator.Candidate.of_domain v
    | Error () -> failwith "Unknown error"
end
