open Sxfiler_core
module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

module type Make = sig
  type params = {
    initial_location: string [@key "initialLocation"];
    name: string;
  } [@@deriving yojson]

  type result = {
    scanner: T.Scanner.t option;
    already_exists: bool;
  }

  val handle: params -> result Lwt.t
end

module Make(System:System.S)(U:Usecase.Scanner.Make) : Make = struct
  type params = {
    initial_location: string [@key "initialLocation"];
    name: string;
  } [@@deriving yojson]

  type result = {
    scanner: T.Scanner.t option;
    already_exists: bool;
  }

  let handle param =
    let params = {
      U.initial_location = Path.of_string param.initial_location |> Path.resolve (module System);
      name = param.name;
    } in
    let empty = {scanner = None; already_exists = false} in
    match%lwt U.execute params with
    | Ok t -> Lwt.return {empty with scanner = Option.some @@ Translator.Scanner.of_domain t}
    | Error e -> begin match e with
        | Usecase.Common.MakeScannerError `Already_exists ->
          Lwt.return {empty with already_exists = true}
        | _ -> assert false
      end
end

module type Get = sig
  type params = {
    name: string;
  } [@@deriving yojson]

  type result = {
    scanner: T.Scanner.t option;
    not_found: bool;
  }

  val handle: params -> result Lwt.t
end

module Get(U:Usecase.Scanner.Get) : Get = struct
  type params = {
    name: string;
  } [@@deriving yojson]

  type result = {
    scanner: T.Scanner.t option;
    not_found: bool;
  }

  let handle param =
    let params = {
      U.name = param.name;
    } in
    match%lwt U.execute params with
    | Ok s -> Lwt.return {scanner = Some (Translator.Scanner.of_domain s); not_found = false;}
    | Error Usecase.Common.GetScannerError `Not_found -> Lwt.return {scanner = None; not_found = true;}
    | _ -> assert false
end
