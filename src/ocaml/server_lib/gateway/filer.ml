open Sxfiler_core
module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

module type Make = sig
  type params =
    { initial_location : string [@key "initialLocation"]
    ; name : string }
  [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; already_exists : bool }

  val handle : params -> result Lwt.t
end

module Make (System : System.S) (U : Usecase.Filer.Make) : Make = struct
  type params =
    { initial_location : string [@key "initialLocation"]
    ; name : string }
  [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; already_exists : bool }

  let handle param =
    let params =
      { U.initial_location = Path.of_string param.initial_location |> Path.resolve (module System)
      ; name = param.name }
    in
    let empty = {filer = None; already_exists = false} in
    match%lwt U.execute params with
    | Ok t -> Lwt.return {empty with filer = Option.some @@ Translator.Filer.of_domain t}
    | Error e -> (
        match e with `Already_exists -> Lwt.return {empty with already_exists = true} )
end

module type Get = sig
  type params = {name : string} [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; not_found : bool }

  val handle : params -> result Lwt.t
end

module Get (U : Usecase.Filer.Get) : Get = struct
  type params = {name : string} [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; not_found : bool }

  let handle param =
    let params = {U.name = param.name} in
    match%lwt U.execute params with
    | Ok s -> Lwt.return {filer = Some (Translator.Filer.of_domain s); not_found = false}
    | Error `Not_found -> Lwt.return {filer = None; not_found = true}
end

(* gateway for Move_parent use case. *)
module type Move_parent = sig
  type params = {name : string} [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; not_found : bool }

  val handle : params -> result Lwt.t
end

module Move_parent (U : Usecase.Filer.Move_parent) : Move_parent = struct
  type params = {name : string} [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; not_found : bool }

  let handle param =
    let params = {U.name = param.name} in
    match%lwt U.execute params with
    | Ok s -> Lwt.return {filer = Some (Translator.Filer.of_domain s); not_found = false}
    | Error `Not_found -> Lwt.return {filer = None; not_found = true}
end

(* gateway for Enter_directory use case. *)
module Enter_directory_type = struct
  type params =
    { name : string
    ; node_id : string [@key "nodeId"] }
  [@@deriving yojson]

  type result =
    { filer : T.Filer.t option
    ; not_found_filer : bool
    ; not_found_node : bool
    ; not_directory : bool }
end

module type Enter_directory = sig
  include module type of Enter_directory_type

  val handle : params -> result Lwt.t
end

module Enter_directory (U : Usecase.Filer.Enter_directory) : Enter_directory = struct
  include Enter_directory_type

  let handle param =
    let params = {U.name = param.name; node_id = param.node_id} in
    let empty =
      {filer = None; not_found_filer = false; not_found_node = false; not_directory = false}
    in
    match%lwt U.execute params with
    | Ok s -> Lwt.return {empty with filer = Some (Translator.Filer.of_domain s)}
    | Error `Not_found_filer -> Lwt.return {empty with not_found_filer = true}
    | Error `Not_found_node -> Lwt.return {empty with not_found_node = true}
    | Error `Not_directory -> Lwt.return {empty with not_directory = true}
end
