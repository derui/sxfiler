(** Filer_op module defines functions for procedures of filer. *)
open Sxfiler_core

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator
module P = Procedure

let make_spec (module Gateway : G.Filer.Make.S) =
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"filer/make"
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let get_spec (module Gateway : G.Filer.Get.S) =
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"filer/get"
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let move_parent_spec (module Gateway : G.Filer.Move_parent.S) =
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"filer/moveParent"
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let enter_directory_spec (module Gateway : G.Filer.Enter_directory.S) =
  let handle params = Gateway.handle params in
  P.to_procedure ~method_:"filer/enterDirectory"
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let make_procedures (module Dep : Dependencies.S) : P.procedure list =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Make_gateway = G.Filer.Make.Make (System.Real) (Dep.Usecase.Filer_make) in
  let module Get_gateway = G.Filer.Get.Make (Dep.Usecase.Filer_get) in
  let module Move_parent_gateway = G.Filer.Move_parent.Make (Dep.Usecase.Filer_move_parent) in
  let module Enter_directory_gateway =
    G.Filer.Enter_directory.Make (Dep.Usecase.Filer_enter_directory) in
  [ make_spec (module Make_gateway)
  ; get_spec (module Get_gateway)
  ; move_parent_spec (module Move_parent_gateway)
  ; enter_directory_spec (module Enter_directory_gateway) ]
