(** This module defines behavior for refresh filer. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

module Make(Service:S.Filer.S) : C.Usecase.S with type param = string = struct
  type t = {
    param: string;
  }

  type param = string

  let create param = {param}

  let execute t dispatcher =

    match%lwt Service.get {name = t.param} with
    | Ok res ->
      let module DI = (val dispatcher: C.Dispatcher_intf.Instance) in
      Lwt.return @@ DI.(Dispatcher.dispatch this C.Message.(Update_filer res))
    | Error `Not_found -> Lwt.return_unit
end
