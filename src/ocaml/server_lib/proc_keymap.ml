(** this module defines functions for procedures for keymap. *)

module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module I = Sxfiler_server_infra

(* defines procedure to get current key bindings *)
module Get (Gateway : G.Keymap.Get) = struct
  include Gateway

  let params_of_json = `Not_required ()
  let result_to_json = T.Key_map.to_yojson
end

(* defines procedure to get current key bindings *)
module Enable_context (G : G.Keymap.Enable_context) = struct
  include G

  let params_of_json = `Required params_of_yojson
  let result_to_json = T.Key_map.to_yojson
end

module Disable_context (G : G.Keymap.Disable_context) = struct
  include G

  let params_of_json = `Required params_of_yojson
  let result_to_json = T.Key_map.to_yojson
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Repo = I.Key_map_repo.Make (Global.Keymap) in
  let module Usecase = U.Keymap.Get (I.Condition_repo.Make (Global.Condition)) (Repo) in
  let module Gateway = G.Keymap.Get (Usecase) in
  let module Get = Procedure_intf.Make (Get (Gateway)) in
  let module Cond_repo = I.Condition_repo.Make (Global.Condition) in
  let module Usecase = U.Keymap.Enable_context (Cond_repo) (Repo) in
  let module Enable_context = Procedure_intf.Make (Enable_context (G.Keymap.Enable_context (Usecase))) in
  let module Usecase = U.Keymap.Disable_context (Cond_repo) (Repo) in
  let module Disable_context =
    Procedure_intf.Make (Disable_context (G.Keymap.Disable_context (Usecase))) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [ (E.Keymap.Get.endpoint, Get.handler)
    ; (E.Keymap.Enable_context.endpoint, Enable_context.handler)
    ; (E.Keymap.Disable_context.endpoint, Disable_context.handler) ]
