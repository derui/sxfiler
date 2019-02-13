(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core

module T = Sxfiler_domain
module Usecase = Sxfiler_usecase
module C = T.Completion
module G = Sxfiler_server_gateway
module P = Procedure
module E = Sxfiler_rpc.Endpoints

let setup_spec (module G : G.Completion.Setup) =
  P.to_procedure ~method_:E.Completion.Setup.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = (fun () -> `Null)
        ; handle = G.handle }

let read_spec (module G : G.Completion.Read) =
  P.to_procedure ~method_:E.Completion.Read.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required G.params_of_yojson
        ; result_to_json = G.result_to_yojson
        ; handle = G.handle }

let initialize completer = Global.Completer.set @@ fun () -> completer

let make_procedures () =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Collection_repo = struct
    let store collection =
      Global.Cached_source.with_lock (fun _ -> Global.Cached_source.update collection)

    let resolve () = Global.Cached_source.get ()
  end in
  let module Setup_gateway = G.Completion.Setup (Usecase.Completion.Setup.Make (Collection_repo)) in
  let module Read_gateway =
    G.Completion.Read
      (Usecase.Completion.Read.Make (Collection_repo) ((val Global.Completer.get ()))) in
  [setup_spec (module Setup_gateway); read_spec (module Read_gateway)]
