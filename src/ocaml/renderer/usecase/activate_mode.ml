(** This usecase to enable context and update keymap. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_service

module Make (Service : S.Keymap.S) : C.Usecase.S with type param = C.Types.Mode.t = struct
  type param = C.Types.Mode.t
  type t = {mode : param}

  let create mode = {mode}

  let execute t dispatcher =
    let context = C.Types.Mode.(to_context t.mode) in
    let other_contexts = C.Types.Mode.(others t.mode |> List.map to_context) in
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    let () = D.(Dispatcher.dispatch this C.Message.(Change_mode t.mode)) in
    let%lwt _ = Lwt_list.map_p (fun v -> Service.disable_context {context = v}) other_contexts in
    let%lwt keymap = Service.enable_context {context} in
    Lwt.return @@ D.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
end
