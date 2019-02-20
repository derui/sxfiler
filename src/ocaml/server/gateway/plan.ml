open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module D = Sxfiler_domain

module Reject = struct
  module Type = struct
    type params = {plan_id : string [@key "planId"]} [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (U : Usecase.Plan.Reject.S) : S = struct
    include Type

    let handle param =
      let params = {U.plan_id = param.plan_id} in
      match%lwt U.execute params with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end

module Filer = struct
  module Make_move_plan = struct
    (* gateway for Plan_move_nodes use case. *)
    module Type = struct
      type params =
        { source : string
        ; node_ids : string list [@key "nodeIds"]
        ; dest : string }
      [@@deriving of_yojson]

      type result = T.Plan.t [@@deriving to_yojson]
    end

    module type S = Core.Gateway with type params = Type.params and type result = Type.result

    module Make (U : Usecase.Plan.Filer.Make_move_plan.S) : S = struct
      include Type

      let handle param =
        match%lwt
          U.execute {source = param.source; node_ids = param.node_ids; dest = param.dest}
        with
        | Ok plan -> T.Plan.of_domain plan |> Lwt.return
        | Error (`Not_found _) -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
        | Error `Same_filer -> Lwt.fail Gateway_error.(Gateway_error plan_same_filer)
    end
  end

  module Make_delete_plan = struct
    (* gateway for Plan_delete_nodes use case. *)
    module Type = struct
      type params =
        { source : string
        ; node_ids : string list [@key "nodeIds"] }
      [@@deriving of_yojson]

      type result = T.Plan.t [@@deriving to_yojson]
    end

    module type S = Core.Gateway with type params = Type.params and type result = Type.result

    module Make (U : Usecase.Plan.Filer.Make_delete_plan.S) : S = struct
      include Type

      let handle param =
        match%lwt U.execute {source = param.source; node_ids = param.node_ids} with
        | Ok plan -> Fun.(T.Plan.of_domain %> Lwt.return) plan
        | Error (`Not_found _) -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
    end
  end
end
