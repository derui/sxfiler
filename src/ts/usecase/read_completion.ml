(** This usecase read completion. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_service

module Make (Service : S.Completion.S) : C.Usecase.S with type param = string = struct
  type param = string
  type t = {input : param}

  let create param = {input = param}

  let execute t dispatcher =
    let module RI = Sxfiler_rpc in
    let%lwt candidates = Service.read {input = t.input} in
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.return @@ D.(Dispatcher.dispatch this C.Message.(Completion (Read candidates)))
end
