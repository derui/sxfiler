module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module SI = Sxfiler_renderer_service_impl

let module_prefix = "global:"

module Activate_file_tree = struct
  let make (module H: Sxfiler_renderer_service.Service_registry.S) =
    {
      Core.Static_command.name = module_prefix ^ "activate_file_tree";
      execute_plan = `No_plan;
      executor = fun _ _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Activate_mode.Make((val H.keymap ())))
            ~param:C.Types.Mode.File_tree) in
        Ctx.(Context.execute this (module I))
    }
end

module Activate_omnibar = struct
  let make (module H: Sxfiler_renderer_service.Service_registry.S) =
    {
      Core.Static_command.name = module_prefix ^ "activate_omnibar";
      execute_plan = `No_plan;
      executor = fun _ _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Activate_mode.Make((val H.keymap ())))
            ~param:C.Types.Mode.Complete) in
        Ctx.(Context.execute this (module I))
    }
end

let expose registry hub =
  List.fold_right (fun command registry ->
      Core.Static_registry.register registry command
    )
    [
      Activate_file_tree.make hub;
      Activate_omnibar.make hub;
    ]
    registry
