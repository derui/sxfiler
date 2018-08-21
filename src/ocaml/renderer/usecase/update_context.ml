(** This usecase to enable context and update keymap. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' = {
  context: string;
  action: [`Add | `Subtract];
}

module Make(Service:S.Keymap.S) : C.Usecase.S with type param = param' = struct
  type param = param'
  type t = {
    param: param';
  }

  let create param = {param = param}

  let execute t dispatcher =

    let module RI = Sxfiler_rpc in
    let%lwt keymap =
      match t.action with
      | `Add -> Service.enable_context {context = t.context}
      | `Subtract -> Service.disable_context {context = t.context}
    in
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.return @@ D.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
end
