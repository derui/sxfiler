(** this module defines functions for procedures for keymap. *)

module Usecase = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module I = Sxfiler_server_infra

(* defines procedure to get current key bindings *)
module Get(Gateway:G.Keymap.Get) = struct
  include Gateway
  let params_of_json = `Not_required ()
  let result_to_json = `Result T.Key_map.to_yojson
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  let module Usecase = Usecase.Keymap.Get(I.Key_map_repo.Make(Global.Keymap)) in
  let module Gateway = G.Keymap.Get(Usecase) in
  let module Get = Procedure_intf.Make(Get(Gateway)) in

  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    E.Keymap.Get.endpoint, Get.handler;
  ]
