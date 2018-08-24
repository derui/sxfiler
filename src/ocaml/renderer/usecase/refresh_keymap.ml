(** This module defines behavior for refresh filer. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

module Make(Service:S.Keymap.S) : C.Usecase.S with type param = unit = struct
  type t = unit

  type param = unit

  let create () = ()

  let execute _ dispatcher =
    let%lwt keymap = Service.get () in
    let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.return @@ DI.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
end
