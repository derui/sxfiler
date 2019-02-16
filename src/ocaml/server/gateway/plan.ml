open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module D = Sxfiler_domain

module Reject = struct
  module Types = struct
    type params = {plan_id : string [@key "planId"]} [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]
  end

  module type S = sig
    include module type of Types

    val handle : params -> result Lwt.t
  end

  module Make (U : Usecase.Plan.Reject.S) : S = struct
    include Types

    let handle param =
      let params = {U.plan_id = param.plan_id} in
      match%lwt U.execute params with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end

module Plan_move_nodes = struct
  (* gateway for Plan_move_nodes use case. *)
  module Types = struct
    type params =
      { source : string
      ; node_ids : string list [@key "nodeIds"]
      ; dest : string }
    [@@deriving of_yojson]
  end

  module type S = sig
    include module type of Types

    val handle : params -> T.Plan.t Lwt.t
  end

  module Make (U : Usecase.Plan.Filer.Make_move_plan.S) : S = struct
    include Types

    let handle param =
      match%lwt
        U.execute {source = param.source; node_ids = param.node_ids; dest = param.dest}
      with
      | Ok plan -> T.Plan.of_domain plan |> Lwt.return
      | Error (`Not_found _) -> Lwt.fail Errors.(Gateway_error plan_not_found_filer)
  end
end

module Plan_delete_nodes = struct
  (* gateway for Plan_delete_nodes use case. *)
  module Types = struct
    type params =
      { source : string
      ; node_ids : string list [@key "nodeIds"] }
    [@@deriving of_yojson]
  end

  module type S = sig
    include module type of Types

    val handle : params -> T.Plan.t Lwt.t
  end

  module Make (U : Usecase.Plan.Filer.Make_delete_plan.S) : S = struct
    include Types

    let handle param =
      match%lwt U.execute {source = param.source; node_ids = param.node_ids} with
      | Ok plan -> Fun.(T.Plan.of_domain %> Lwt.return) plan
      | Error (`Not_found _) -> Lwt.fail Errors.(Gateway_error plan_not_found_filer)
  end
end
