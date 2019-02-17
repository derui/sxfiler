(** this module defines functions for procedures for keybindings. *)

module Usecase = Sxfiler_usecase
module G = Sxfiler_server_gateway
module I = Sxfiler_server_infra
module T = Sxfiler_server_translator
module P = Procedure

(* defines procedure to get current key bindings *)
let get_spec (module G : G.Configuration.Get.S) =
  P.to_procedure ~method_:"configuration/get"
    ~spec:
      P.Spec.
        { params_of_json = `Not_required ()
        ; result_to_json = T.Configuration.to_yojson
        ; handle = G.handle }

let make_procedures (module Dep : Dependencies.S) =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Gateway = G.Configuration.Get.Make (Dep.Usecase.Configuration_get) in
  [get_spec (module Gateway)]
