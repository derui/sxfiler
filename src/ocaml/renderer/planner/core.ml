module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_store

module type Action = sig
  val state : unit -> S.App.State.t
end

type corrections = T.Node.t list

(** [plan] defines type to make plan with planner.  *)
type plan =
  corrections -> dispatcher:(module C.Dispatcher.Instance) -> action:(module Action) -> unit Lwt.t

(** [execute] defines type to execute plan.  *)
type execute = dispatcher:(module C.Dispatcher.Instance) -> action:(module Action) -> unit Lwt.t

type executor =
  { plan : plan
  ; execute : execute }
