(** this module defines functions for procedures for keybindings. *)

open Sxfiler_server_core
module Rpc = Sxfiler_rpc
module T = Sxfiler_domain
module G = Sxfiler_server_gateway

(* defines procedure to get current key bindings *)
module Get_sync
    (State:Statable.S with type state = T.Configuration.t) = Procedure_intf.Make(struct
    include Rpc.Configuration.Get_sync

    let params_of_json = `Not_required ()
    let result_to_json = `Result G.Configuration.Get_sync.result_to_yojson

    let handle () = State.get ()
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc.Configuration in

  let module Get_sync = Get_sync(Global.Configuration) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Get_sync.name, Get_sync.handler;
  ]
