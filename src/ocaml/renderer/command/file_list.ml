module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module B = Sxfiler_renderer_behavior

let module_prefix = "file_list:"

module Next_item = struct
  let make () =

    let module Command = struct
        type t = unit
        type store = S.App.Store.t
        let name () = module_prefix ^ "next_item"
        let param_defs () = []

        let plan = `No_plan
        let execute () _ (module Ctx : C.Context.Instance) =
          let module I = (val C.Behavior.make_instance (module B.Move_scanner_cursor) ~config:() ~param:`Next) in
          Lwt.return @@ Ctx.(Context.execute this (module I))
      end
    in

    (module struct
      type store = S.App.Store.t
      module Command = Command
      let this = ()
    end : C.Command.Instance with type store = S.App.Store.t)
end

module Prev_item = struct
  let make () =

    let module Command = struct
        type t = unit
        type store = S.App.Store.t
        let name () = module_prefix ^ "next_item"
        let param_defs () = []

        let plan = `No_plan
        let execute () _ (module Ctx : C.Context_intf.Instance) =
          let module I = (val C.Behavior.make_instance (module B.Move_scanner_cursor) ~config:() ~param:`Prev) in
          Lwt.return @@ Ctx.(Context.execute this (module I))
      end
    in

    (module struct
      type store = S.App.Store.t
      module Command = Command
      let this = ()
    end : C.Command.Instance with type store = S.App.Store.t)
end
