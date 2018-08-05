module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module B = Sxfiler_renderer_behavior

let module_prefix = "file_list:"

module Next_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "next_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Behavior.make_instance (module B.Move_scanner_cursor) ~config:() ~param:`Next) in
        Ctx.(Context.execute this (module I))
    }
end

module Prev_item = struct
  let make () =
    {
      C.Command.Static_command.name = module_prefix ^ "prev_item";
      execute_plan = `No_plan;
      executor = fun _ (module Ctx : C.Context.Instance) ->
        let module I = (val C.Behavior.make_instance (module B.Move_scanner_cursor) ~config:() ~param:`Prev) in
        Ctx.(Context.execute this (module I))
    }
end

let expose registry =
  List.fold_right (fun command registry ->
      C.Locator_intf.Static_registry.register registry command
    )
    [
      Next_item.make ();
      Prev_item.make ();
    ]
    registry
