open Sxfiler_core
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module SI = Sxfiler_renderer_service_impl

let module_prefix = "global:"

module Activate_content = struct
  let make (module H : Sxfiler_renderer_service.Service_registry.S) =
    { Core.name = module_prefix ^ "activate_content"
    ; executor =
        Immediate
          (fun _ state (module Ctx : C.Context.Instance) ->
             let current_content =
               Fun.(
                 S.App.State.workspace %> S.Workspace.Store.get %> S.Workspace.State.current_content)
                 state
             in
             let instance =
               C.Usecase.make_instance
                 (module U.Activate_mode.Make ((val H.keymap ())))
                 ~param:C.Types.Mode.(Content current_content)
             in
             Ctx.(Context.execute this instance) ) }
end

module Activate_omnibar = struct
  let make (module H : Sxfiler_renderer_service.Service_registry.S) =
    { Core.name = module_prefix ^ "activate_omnibar"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let instance =
               C.Usecase.make_instance
                 (module U.Activate_mode.Make ((val H.keymap ())))
                 ~param:C.Types.Mode.Complete
             in
             Ctx.(Context.execute this instance) ) }
end

let expose registry hub =
  List.fold_right
    (fun command registry -> Core.Static_registry.register registry command)
    [Activate_content.make hub; Activate_omnibar.make hub]
    registry
