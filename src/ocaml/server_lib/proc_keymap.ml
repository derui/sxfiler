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
module Add_context (G : G.Keymap.Add_context.S) = struct
  include G

  let params_of_json = `Required params_of_yojson
  let result_to_json = T.Key_map.to_yojson
end

module Delete_context (G : G.Keymap.Delete_context.S) = struct
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
  let module Usecase = U.Keymap.Add_context.Make (Cond_repo) (Repo) in
  let module Add_context = Procedure_intf.Make (Add_context (G.Keymap.Add_context.Make (Usecase))) in
  let module Usecase = U.Keymap.Delete_context.Make (Cond_repo) (Repo) in
  let module Delete_context =
    Procedure_intf.Make (Delete_context (G.Keymap.Delete_context.Make (Usecase))) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [ (E.Keymap.Get.endpoint, Get.handler)
    ; (E.Keymap.Add_context.endpoint, Add_context.handler)
    ; (E.Keymap.Delete_context.endpoint, Delete_context.handler) ]
