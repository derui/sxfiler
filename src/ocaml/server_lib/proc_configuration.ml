(** this module defines functions for procedures for keybindings. *)

module Usecase = Sxfiler_usecase
module G = Sxfiler_server_gateway
module I = Sxfiler_server_infra
module T = Sxfiler_server_translator

(* defines procedure to get current key bindings *)
module Get (G : G.Configuration.Get) = struct
  include G

  let params_of_json = `Not_required ()
  let result_to_json = T.Configuration.to_yojson
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Configuration in
  let module R = I.Configuration_repo.Make (Global.Root) in
  let module Usecase = Usecase.Configuration.Get (R) in
  let module Gateway = G.Configuration.Get (Usecase) in
  let module Get = Procedure_intf.Make (Get (Gateway)) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [(E.Configuration.Get.endpoint, Get.handler)]
