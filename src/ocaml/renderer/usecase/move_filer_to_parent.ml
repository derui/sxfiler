(** This module defines behavior to move filer to new location that is parent of it.  *)
open Sxfiler_core
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' = C.Types.File_list_pos.t

module Make(Service:S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {
    param: param';
  }

  type param = param'

  let create param = {param}

  let execute t dispatcher =

    let name = C.Types.File_list_pos.to_string t.param in
    let%lwt message =
      try%lwt
        let%lwt res = Service.move_parent {name} in
        Lwt.return C.Message.(Update_filer (t.param, res))
      with Error.Error t -> Lwt.return C.Message.(Raise_error t)
    in
    let module DI = (val dispatcher: C.Dispatcher_intf.Instance) in
    Lwt.return DI.(Dispatcher.dispatch this message)
end
