(** Proc_plan module defines functions for procedures of plan. *)

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator
module P = Procedure

let reject_spec (module Gateway : G.Plan.Reject.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json _ = `Null in
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"plan/reject" ~spec:P.Spec.{params_of_json; result_to_json; handle}

let plan_move_nodes_spec (module Gateway : G.Plan.Plan_move_nodes.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"plan/filer/moveNodes"
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let plan_delete_nodes_spec (module Gateway : G.Plan.Plan_delete_nodes.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"plan/filer/deleteNodes"
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let make_procedures () =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Filer in
  let module I = Sxfiler_server_infra in
  let module Filer_repo = I.Filer_repo.Make (Global.Root) in
  let module Conf_repo = I.Configuration_repo.Make (Global.Root) in
  let module Reject_gateway = G.Plan.Reject.Make (U.Plan.Reject.Make (Wb_repo)) in
  let module Workbench_make = U.Workbench.Make (Filer_repo) (Wb_factory) (Wb_repo) in
  let module Plan_move_nodes_gateway =
    G.Filer.Plan_move_nodes.Make (Workbench_make) (U.Plan.Filer.Move_nodes.Make (Wb_repo))
  in
  let module Plan_delete_nodes_gateway =
    G.Filer.Plan_delete_nodes.Make (Workbench_make) (U.Plan.Filer.Delete_nodes.Make (Wb_repo))
  in
  [ reject_spec (module Reject_gateway)
  ; plan_move_nodes_spec (module Plan_move_nodes_gateway)
  ; plan_delete_nodes_spec (module Plan_delete_nodes_gateway) ]
