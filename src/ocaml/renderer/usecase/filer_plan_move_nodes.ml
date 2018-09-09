(** This module defines behavior to move filer to new location that is child directory in filer.  *)
open Sxfiler_core

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' =
  { from : C.Types.File_list_pos.t
  ; nodes : T.Node.t list
  ; _to : C.Types.File_list_pos.t }

let plan_name = "move_nodes"

module Make (Service : S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let from = C.Types.File_list_pos.to_string t.param.from
    and _to = C.Types.File_list_pos.to_string t.param._to
    and node_ids = List.map (fun node -> node.T.Node.id) t.param.nodes in
    let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
    DI.(Dispatcher.dispatch this C.Message.(Command Planning)) ;
    let%lwt message =
      try%lwt
        let%lwt res = Service.plan_move_nodes {from; node_ids; _to} in
        Lwt.return C.Message.(Command (Plan res))
      with Error.Error t -> Lwt.return C.Message.(Raise_error t)
    in
    Lwt.return DI.(Dispatcher.dispatch this message)
end
