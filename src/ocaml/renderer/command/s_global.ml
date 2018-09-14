module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module SI = Sxfiler_renderer_service_impl

let module_prefix = "global:"

module Focus_omnibar = struct
  let make (module H : Sxfiler_renderer_service.Service_registry.S) =
    { Core.name = module_prefix ^ "focus_omnibar"
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

module Exit_omnibar = struct
  let make (module H : Sxfiler_renderer_service.Service_registry.S) =
    { Core.name = module_prefix ^ "exit_omnibar"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let instance = C.Usecase.make_instance (module U.Deactivate_mode) ~param:() in
             Ctx.(Context.execute this instance) ) }
end

let expose registry hub =
  List.fold_right
    (fun command registry -> Core.Static_registry.register registry command)
    [Focus_omnibar.make hub; Exit_omnibar.make hub]
    registry
