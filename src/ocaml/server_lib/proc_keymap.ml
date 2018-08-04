(** this module defines functions for procedures for keymap. *)

open Sxfiler_server_core
module Rpcy = Sxfiler_rpc_yojson
module D = Sxfiler_domain

(* defines procedure to get current key bindings *)
module Get_sync
    (Keymap:Statable.S with type state = string D.Key_map.t) = Procedure_intf.Make(struct
    include Rpcy.Keymap.Get_sync

    let params_of_json = `Not_required ()
    let result_to_json = `Result result_to_yojson

    let handle () = Keymap.get ()
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc_yojson.Keymap in

  let module Get_sync = Get_sync(Global.Keymap) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Get_sync.name, Get_sync.handler;
  ]
