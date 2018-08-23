module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase

let module_prefix = "completion:"

module Next_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "next_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module B = (val C.Usecase.make_instance (module U.Select_next_candidate)
                           ~param:()) in
        Ctx.(Context.execute this (module B))
    }
end

module Prev_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "prev_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Select_prev_candidate) ~param:()) in
        Ctx.(Context.execute this (module I))
    }
end

let expose registry =
  List.fold_right (fun command registry ->
      C.Command.Static_registry.register registry command
    )
    [
      Next_item.make ();
      Prev_item.make ();
    ]
    registry
