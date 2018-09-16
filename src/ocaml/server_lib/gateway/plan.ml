open Sxfiler_core
module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types
module D = Sxfiler_domain

module Reject = struct
  module Types = struct
    type params = {workbench_id : string [@key "workbenchId"]} [@@deriving yojson]

    type result =
      { invalid_id : bool
      ; unknown_error : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (U : Usecase.Plan.Reject.S) : S = struct
    include Types

    let handle param =
      let result = {invalid_id = false; unknown_error = false} in
      match Uuidm.of_string param.workbench_id with
      | None -> Lwt.return {result with invalid_id = true}
      | Some workbench_id -> (
          let params = {U.workbench_id} in
          match%lwt U.execute params with
          | Ok () -> Lwt.return result
          | Error () -> Lwt.return {result with unknown_error = true} )
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

  module Make (WB : Usecase.Workbench.Make) (U : Usecase.Plan.Filer.Move_nodes.S) : S = struct
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
