open Sxfiler_core
module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types
module D = Sxfiler_domain

module Make = struct
  module Types = struct
    type params =
      { initial_location : string [@key "initialLocation"]
      ; name : string }
    [@@deriving yojson]

    type result =
      { filer : T.Filer.t option
      ; already_exists : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (System : System.S) (U : Usecase.Filer.Make) : S = struct
    include Types

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
end

module Get = struct
  module Types = struct
    type params = {name : string} [@@deriving yojson]

    type result =
      { filer : T.Filer.t option
      ; not_found : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (U : Usecase.Filer.Get) : S = struct
    include Types

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return {filer = Some (Translator.Filer.of_domain s); not_found = false}
      | Error `Not_found -> Lwt.return {filer = None; not_found = true}
  end
end

module Move_parent = struct
  (* gateway for Move_parent use case. *)
  module Types = struct
    type params = {name : string} [@@deriving yojson]

    type result =
      { filer : T.Filer.t option
      ; not_found : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (U : Usecase.Filer.Move_parent) : S = struct
    include Types

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return {filer = Some (Translator.Filer.of_domain s); not_found = false}
      | Error `Not_found -> Lwt.return {filer = None; not_found = true}
  end
end

(* gateway for Enter_directory use case. *)
module Enter_directory = struct
  module Types = struct
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

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (U : Usecase.Filer.Enter_directory) : S = struct
    include Types

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
end

module Plan_move_nodes = struct
  (* gateway for Plan_move_nodes use case. *)
  module Types = struct
    type params =
      { from : string
      ; node_ids : string list [@key "nodeIds"]
      ; _to : string [@key "to"] }
    [@@deriving yojson]

    type result =
      { plan : T.Plan.t option
      ; not_found_filer : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (WB : Usecase.Workbench.Make) (U : Usecase.Filer.Plan_move_nodes.S) : S = struct
    include Types

    let handle param =
      let empty = {plan = None; not_found_filer = false} in
      let params = {WB.from = param.from; node_ids = param.node_ids; _to = param._to} in
      match%lwt WB.execute params with
      | Ok wb -> (
          match%lwt U.execute {workbench_id = wb.D.Workbench.id} with
          | Ok plan ->
            Lwt.return {empty with plan = Fun.(Translator.Plan.of_domain %> Option.some) plan}
          | Error _ -> assert false )
      | Error `Not_found_filer -> Lwt.return {empty with not_found_filer = true}
  end
end
