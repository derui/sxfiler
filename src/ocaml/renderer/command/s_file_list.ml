module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module Svc = Sxfiler_renderer_service

let module_prefix = "file_list:"

module Next_item = struct
  let make () =
    { Core.name = module_prefix ^ "next_item"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let module I =
               (val C.Usecase.make_instance (module U.Move_filer_cursor) ~param:`Next)
             in
             Ctx.(Context.execute this (module I)) ) }
end

module Prev_item = struct
  let make () =
    { Core.name = module_prefix ^ "prev_item"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let module I =
               (val C.Usecase.make_instance (module U.Move_filer_cursor) ~param:`Prev)
             in
             Ctx.(Context.execute this (module I)) ) }
end

module Swap_filer = struct
  let make () =
    { Core.name = module_prefix ^ "swap_filer"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let module I = (val C.Usecase.make_instance (module U.Swap_filer) ~param:()) in
             Ctx.(Context.execute this (module I)) ) }
end

module Move_parent = struct
  let make (module Reg : Svc.Service_registry.S) =
    { Core.name = module_prefix ^ "move_parent"
    ; executor =
        Immediate
          (fun _ state (module Ctx : C.Context.Instance) ->
             let current_filer = S.(File_list.Store.get @@ S.App.State.file_list state).current in
             let module I =
               ( val C.Usecase.make_instance
                   (module U.Move_filer_to_parent.Make ((val Reg.filer ())))
                   ~param:current_filer )
             in
             Ctx.(Context.execute this (module I)) ) }
end

(* the command toggles mark of item in current filer.  *)
module Toggle_mark = struct
  let make () =
    { Core.name = module_prefix ^ "toggle_mark"
    ; executor =
        Immediate
          (fun _ _ (module Ctx : C.Context.Instance) ->
             let module I = (val C.Usecase.make_instance (module U.Filer_toggle_mark) ~param:()) in
             Ctx.(Context.execute this (module I)) ) }
end

module Enter_directory = struct
  let make (module Reg : Svc.Service_registry.S) =
    { Core.name = module_prefix ^ "enter_directory"
    ; executor =
        Immediate
          (fun _ state (module Ctx : C.Context.Instance) ->
             let file_list = S.(App.State.file_list state |> File_list.Store.get) in
             let pos = file_list.current in
             let node =
               let open Sxfiler_core.Option.Infix in
               S.File_list.(State.current file_list >|= Filer.current_selected_node)
             in
             match node with
             | None -> Lwt.fail_with "Not initialized yet"
             | Some node ->
               let module I =
                 ( val C.Usecase.make_instance
                     (module U.Filer_enter_directory.Make ((val Reg.filer ())))
                     ~param:{pos; node} )
               in
               Ctx.(Context.execute this (module I)) ) }
end

let expose registry services =
  List.fold_right
    (fun command registry -> Core.Static_registry.register registry command)
    [ Next_item.make ()
    ; Prev_item.make ()
    ; Swap_filer.make ()
    ; Move_parent.make services
    ; Toggle_mark.make ()
    ; Enter_directory.make services ]
    registry
