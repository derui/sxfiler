(** this module defines functions for procedures for keybindings. *)

module Usecase = Sxfiler_usecase
module G = Sxfiler_server_gateway
module I = Sxfiler_server_infra
module T = Sxfiler_server_translator
module P = Procedure_intf
module E = Sxfiler_rpc.Endpoints

(* defines procedure to get current key bindings *)
let get_spec (module G : G.Configuration.Get) =
  P.to_procedure ~method_:E.Configuration.Get.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Not_required ()
        ; result_to_json = T.Configuration.to_yojson
        ; handle = G.handle }

let make_procedures () =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Configuration in
  let module R = I.Configuration_repo.Make (Global.Root) in
  let module Usecase = Usecase.Configuration.Get.Make (R) in
  let module Gateway = G.Configuration.Get (Usecase) in
  [get_spec (module Gateway)]
