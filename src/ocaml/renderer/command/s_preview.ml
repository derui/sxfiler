open Sxfiler_core
module C = Sxfiler_renderer_core
module U = Sxfiler_renderer_usecase
module S = Sxfiler_renderer_store
module Svc = Sxfiler_renderer_service

let module_prefix = "preview:"

module Reject = struct
  let make (module Svc : Svc.Service_registry.S) =
    { Core.name = module_prefix ^ "reject"
    ; executor =
        Immediate
          (fun _ state (module Ctx : C.Context.Instance) ->
             let command = Fun.(S.App.State.command %> S.Command.Store.get) state in
             match command.plan with
             | None -> Lwt.return_unit
             | Some plan ->
               let instance =
                 C.Usecase.make_instance
                   (module U.Reject_current_plan.Make ((val Svc.plan ())))
                   ~param:plan
               in
               Ctx.(Context.execute this instance) ) }
end

module Approve = struct
  let make () =
    { Core.name = module_prefix ^ "approve"
    ; executor =
        Immediate
          (fun _ state (module Ctx : C.Context.Instance) ->
             let command = Fun.(S.App.State.command %> S.Command.Store.get) state in
             if Option.is_some command.plan then
               let instance = C.Usecase.make_instance (module U.Approve_current_plan) ~param:() in
               Ctx.(Context.execute this instance)
             else Lwt.return_unit ) }
end

let expose registry service =
  List.fold_right
    (fun command registry -> Core.Static_registry.register registry command)
    [Reject.make service; Approve.make ()]
    registry
