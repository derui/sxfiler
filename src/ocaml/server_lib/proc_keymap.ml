(** this module defines functions for procedures for keymap. *)

open Sxfiler_server_core
module Rpc = Sxfiler_rpc
module G = Sxfiler_server_gateway
module D = Sxfiler_domain

(* defines procedure to get current key bindings *)
module Get_sync
    (Keymap:Statable.S with type state = string D.Key_map.t) = Procedure_intf.Make(struct
    include Rpc.Keymap.Get_sync

    let params_of_json = `Not_required ()
    let result_to_json = `Result G.Keymap.Get_sync.result_to_yojson

    let handle () = Keymap.get ()
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  let module Get_sync = Get_sync(Global.Keymap) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    Rpc.Keymap.Get_sync.name, Get_sync.handler;
  ]
