(** this module defines functions for procedures for keybindings. *)

open Sxfiler_core
open Sxfiler_server_core
module Rpcy = Sxfiler_rpc_yojson

(* defines procedure to get current key bindings *)
module Get_sync
    (Keybindings:Statable.S with type state = Yojson.Safe.json) = Procedure_intf.Make(struct
    include Rpcy.Keybindings.Get_sync

    let params_of_json = `Not_required ()
    let result_to_json = `Result Fun.ident

    let handle () = Keybindings.get ()
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc_yojson.Keybindings in

  let module Get_sync = Get_sync(Global.Keybindings) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Get_sync.name, Get_sync.handler;
  ]
