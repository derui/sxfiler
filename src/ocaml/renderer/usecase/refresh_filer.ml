(** This module defines behavior for refresh filer. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_service

type param' = C.Types.File_list_pos.t

module Make (Service : S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let name = C.Types.File_list_pos.to_string t.param in
    match%lwt Service.get {name} with
    | Ok res ->
      let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
      Lwt.return @@ DI.(Dispatcher.dispatch this C.Message.(Update_filer (t.param, res)))
    | Error `Not_found ->
      Lwt.return_unit
end
