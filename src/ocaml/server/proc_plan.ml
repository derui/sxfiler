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

let plan_move_nodes_spec (module Gateway : G.Plan.Filer.Make_move_plan.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"plan/filer/moveNodes"
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let plan_delete_nodes_spec (module Gateway : G.Plan.Filer.Make_delete_plan.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"plan/filer/deleteNodes"
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let make_procedures (module Dep : Dependencies.S) =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Reject_gateway = G.Plan.Reject.Make (Dep.Usecase.Plan_reject) in
  let module Plan_move_nodes_gateway =
    G.Plan.Filer.Make_move_plan.Make (Dep.Usecase.Plan_filer_make_move_plan) in
  let module Plan_delete_nodes_gateway =
    G.Plan.Filer.Make_delete_plan.Make (Dep.Usecase.Plan_filer_make_delete_plan) in
  [ reject_spec (module Reject_gateway)
  ; plan_move_nodes_spec (module Plan_move_nodes_gateway)
  ; plan_delete_nodes_spec (module Plan_delete_nodes_gateway) ]
