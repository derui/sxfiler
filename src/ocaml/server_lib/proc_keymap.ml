(** this module defines functions for procedures for keymap. *)

module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module I = Sxfiler_server_infra
module P = Procedure
module E = Sxfiler_rpc.Endpoints

(* defines procedure to get current key bindings *)
let get_spec (module Gateway : G.Keymap.Get) =
  P.to_procedure ~method_:E.Keymap.Get.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Not_required ()
        ; result_to_json = T.Key_map.to_yojson
        ; handle = Gateway.handle }

(* defines procedure to get current key bindings *)
let add_context_spec (module G : G.Keymap.Add_context.S) =
  P.to_procedure ~method_:E.Keymap.Add_context.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = T.Key_map.to_yojson
        ; handle = G.handle }

let delete_context_spec (module G : G.Keymap.Delete_context.S) =
  P.to_procedure ~method_:E.Keymap.Delete_context.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = T.Key_map.to_yojson
        ; handle = G.handle }

let make_procedures () : P.procedure list =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Repo = I.Key_map_repo.Make (Global.Keymap) in
  let module Usecase = U.Keymap.Get.Make (I.Condition_repo.Make (Global.Condition)) (Repo) in
  let module Get_gateway = G.Keymap.Get (Usecase) in
  let module Cond_repo = I.Condition_repo.Make (Global.Condition) in
  let module Usecase = U.Keymap.Add_context.Make (Cond_repo) (Repo) in
  let module Add_context_gateway = G.Keymap.Add_context.Make (Usecase) in
  let module Usecase = U.Keymap.Delete_context.Make (Cond_repo) (Repo) in
  let module Delete_context_gateway = G.Keymap.Delete_context.Make (Usecase) in
  [ get_spec (module Get_gateway)
  ; add_context_spec (module Add_context_gateway)
  ; delete_context_spec (module Delete_context_gateway) ]
