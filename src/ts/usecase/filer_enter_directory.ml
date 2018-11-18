(** This module defines behavior to move filer to new location that is child directory in filer.  *)
open Sxfiler_core

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' =
  { pos : C.Types.File_list_pos.t
  ; node : T.Node.t }

module Make (Service : S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let name = C.Types.File_list_pos.to_string t.param.pos and node_id = t.param.node.id in
    let%lwt message =
      try%lwt
        let%lwt res = Service.enter_directory {name; node_id} in
        Lwt.return C.Message.(Update_filer (t.param.pos, res))
      with Error.Error t -> Lwt.return C.Message.(Raise_error t)
    in
    let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.return DI.(Dispatcher.dispatch this message)
end
