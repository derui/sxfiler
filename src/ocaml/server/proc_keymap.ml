(** this module defines functions for procedures for keymap. *)

module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module I = Sxfiler_server_infra
module P = Procedure

(* defines procedure to get current key bindings *)
let get_spec (module Gateway : G.Keymap.Get) =
  P.to_procedure ~method_:"keymap/get"
    ~spec:
      { P.Spec.params_of_json = `Not_required ()
      ; result_to_json = T.Key_map.to_yojson
      ; handle = Gateway.handle }

(* defines procedure to get current key bindings *)
let add_context_spec (module G : G.Keymap.Add_context.S) =
  P.to_procedure ~method_:"keymap/addContext"
    ~spec:
      { P.Spec.params_of_json = `Required G.params_of_yojson
      ; result_to_json = T.Key_map.to_yojson
      ; handle = G.handle }

let delete_context_spec (module G : G.Keymap.Delete_context.S) =
  P.to_procedure ~method_:"keymap/deleteContext"
    ~spec:
      { P.Spec.params_of_json = `Required G.params_of_yojson
      ; result_to_json = T.Key_map.to_yojson
      ; handle = G.handle }

let make_procedures (module Dep : Dependencies.S) : P.procedure list =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Get_gateway = G.Keymap.Get (Dep.Usecase.Keymap_get) in
  let module Add_context_gateway = G.Keymap.Add_context.Make (Dep.Usecase.Keymap_add_context) in
  let module Delete_context_gateway =
    G.Keymap.Delete_context.Make (Dep.Usecase.Keymap_delete_context) in
  [ get_spec (module Get_gateway)
  ; add_context_spec (module Add_context_gateway)
  ; delete_context_spec (module Delete_context_gateway) ]
