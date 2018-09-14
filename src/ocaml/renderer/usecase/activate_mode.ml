(** This usecase to enable context and update keymap. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_service

module Make (Service : S.Keymap.S) : C.Usecase.S with type param = C.Types.Mode.t = struct
  type param = C.Types.Mode.t
  type t = {mode : param}

  let create mode = {mode}

  let execute t dispatcher =
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.return D.(Dispatcher.dispatch this C.Message.(Focus_mode t.mode))
end
