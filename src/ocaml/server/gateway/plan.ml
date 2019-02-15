open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module D = Sxfiler_domain

module Reject = struct
  module Types = struct
    type params = {plan_id : string [@key "planId"]} [@@deriving yojson]

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
      let params = {U.plan_id = param.plan_id} in
      match%lwt U.execute params with
      | Ok () -> Lwt.return result
      | Error () -> Lwt.return {result with unknown_error = true}
  end
end

module Plan_move_nodes = struct
  (* gateway for Plan_move_nodes use case. *)
  module Types = struct
    type params =
      { source : string
      ; node_ids : string list [@key "nodeIds"]
      ; dest : string }
    [@@deriving yojson]

    type error = [`Not_found_filer]
  end

  module type S = sig
    include module type of Types

    val handle : params -> (T.Plan.t, error) result Lwt.t
  end

  module Make (U : Usecase.Plan.Filer.Make_move_plan.S) : S = struct
    include Types

    let handle param =
      match%lwt
        U.execute {source = param.source; node_ids = param.node_ids; dest = param.dest}
      with
      | Ok plan -> T.Plan.of_domain plan |> Lwt.return_ok
      | Error (`Not_found _) -> Lwt.return_error `Not_found_filer
  end
end

module Plan_delete_nodes = struct
  (* gateway for Plan_delete_nodes use case. *)
  module Types = struct
    type params =
      { from : string
      ; node_ids : string list [@key "nodeIds"] }
    [@@deriving yojson]

    type result =
      { plan : T.Plan.t option
      ; not_found_filer : bool }
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (WB : Usecase.Workbench.Make) (U : Usecase.Plan.Filer.Delete_nodes.S) : S = struct
    include Types

    let handle param =
      let empty = {plan = None; not_found_filer = false} in
      let params = {WB.from = param.from; node_ids = param.node_ids; _to = param.from} in
      match%lwt WB.execute params with
      | Ok wb -> (
          match%lwt U.execute {workbench_id = wb.D.Workbench.id} with
          | Ok plan ->
            Lwt.return {empty with plan = Fun.(Translator.Plan.of_domain %> Option.some) plan}
          | Error _ -> assert false )
      | Error `Not_found_filer -> Lwt.return {empty with not_found_filer = true}
  end
end
