(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core

module T = Sxfiler_domain
module Usecase = Sxfiler_usecase
module C = T.Completion
module G = Sxfiler_server_gateway
module P = Procedure

let setup_spec (module G : G.Completion.Setup.S) =
  P.to_procedure ~method_:"completion/setup"
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = (fun () -> `Null)
        ; handle = G.handle }

let read_spec (module G : G.Completion.Read.S) =
  P.to_procedure ~method_:"completion/read"
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = G.result_to_yojson
        ; handle = G.handle }

let initialize completer = Global.Completer.set @@ fun () -> completer

let make_procedures (module Dep : Dependencies.S) =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Setup_gateway = G.Completion.Setup.Make (Dep.Usecase.Completion_setup) in
  let module Read_gateway = G.Completion.Read.Make (Dep.Usecase.Completion_read) in
  [setup_spec (module Setup_gateway); read_spec (module Read_gateway)]
