module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase

let module_prefix = "file_list:"

module Next_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "next_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Move_scanner_cursor) ~param:`Next) in
        Ctx.(Context.execute this (module I))
    }
end

module Prev_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "prev_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Move_scanner_cursor) ~param:`Prev) in
        Ctx.(Context.execute this (module I))
    }
end

module Swap_scanner = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "swap_scanner";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Usecase.make_instance (module U.Swap_scanner) ~param:()) in
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
      Swap_scanner.make ();
    ]
    registry
