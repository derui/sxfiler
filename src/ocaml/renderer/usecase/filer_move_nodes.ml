(** This module defines use case to make plan to move nodes between filers.  *)
open Sxfiler_core

module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' =
  { from : C.Types.File_list_pos.t
  ; node_ids : string list
  ; _to : C.Types.File_list_pos.t }

module Make (Service : S.Plan.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
    DI.(Dispatcher.dispatch this C.Message.(Command Planning)) ;
    let from_name = C.Types.File_list_pos.to_string t.param.from
    and to_name = C.Types.File_list_pos.to_string t.param._to in
    let%lwt message =
      try%lwt
        let%lwt res =
          Service.plan_move_nodes {from = from_name; node_ids = t.param.node_ids; _to = to_name}
        in
        Lwt.return C.Message.(Command (Plan res))
      with Error.Error t -> Lwt.return C.Message.(Raise_error t)
    in
    Lwt.return DI.(Dispatcher.dispatch this message)
end
