(** this module defines functions for procedures for keybindings. *)

open Sxfiler_server_core
module Runner = Sxfiler_server_task.Runner
module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson
module Act = Sxfiler_server_action

(* defines procedure to get current key bindings *)
module Get_sync(Action:Act.Action_intf.Instance)
    (State:Statable.S with type state = T.Configuration.t) = Procedure_intf.Make(struct
    include Rpcy.Configuration.Get_sync

    let params_of_json = `Not_required ()
    let result_to_json = `Result Ty.Configuration.to_yojson

    let handle () = State.get ()
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc.Scanner in

  let module Runner = (val Global.Task_runner.get (): Runner.Instance) in
  let module Get_sync = Get_sync(Act.Real)(Global.Configuration) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Get_sync.name, Get_sync.handler;
  ]
