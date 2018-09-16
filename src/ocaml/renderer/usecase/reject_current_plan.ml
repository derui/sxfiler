(** This use case do to reject current plan.  *)

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

module Make (Svc : S.Plan.S) : C.Usecase.S with type param = T.Plan.t = struct
  type param = T.Plan.t
  type t = {param : param}

  let create param = {param}

  let execute t dispatcher =
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    Lwt.(
      Svc.reject {workbench_id = t.param.workbench_id}
      >>= fun () -> Lwt.return D.(Dispatcher.dispatch this C.Message.(Command Reject)))
end
