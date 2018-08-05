(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core
module T = Sxfiler_domain
module Usecase = Sxfiler_usecase
module C = Sxfiler_completion
module CM = Sxfiler_completion_migemo
module G = Sxfiler_server_gateway

module Setup(G:G.Completion.Setup) = struct
  include G
  let params_of_json = `Required G.params_of_yojson
  let result_to_json = `Void
end

module Read(G:G.Completion.Read) = struct
  include G
  let params_of_json = `Required G.params_of_yojson
  let result_to_json = `Result G.result_to_yojson
end

let initialize migemo =
  Global.Completer.set @@ fun () -> (CM.Completer.make ~migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  let module Collection_repo = struct
    let store collection = Global.Cached_source.with_lock (fun _ ->
        Global.Cached_source.update collection
      )

    let resolve () = Global.Cached_source.get ()
  end in

  let module Setup_gateway = G.Completion.Setup(C.Usecase.Setup(Collection_repo)) in
  let module Setup = Procedure_intf.Make(Setup(Setup_gateway)) in
  let module Read_gateway = G.Completion.Read(
      C.Usecase.Read(Collection_repo)(val Global.Completer.get ())) in
  let module Read = Procedure_intf.Make(Read(Read_gateway)) in

  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    (E.Completion.Setup.endpoint, Setup.handler);
    (E.Completion.Read.endpoint, Read.handler);
  ]
