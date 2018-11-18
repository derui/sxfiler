(** This use case do to approve current plan.  *)

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param = T.Plan.t
type t = {param : param}

let create param = {param}

let execute t dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  let conflict_remained =
    List.exists
      (fun v -> match v.T.Plan.operation with Conflict -> true | _ -> false)
      t.param.T.Plan.source
  in
  if conflict_remained then
    Lwt.return D.(Dispatcher.dispatch this C.Message.(Command Remains_conflict))
  else Lwt.return D.(Dispatcher.dispatch this C.Message.(Command Approve))
