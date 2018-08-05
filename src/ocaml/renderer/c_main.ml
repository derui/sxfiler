module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful (struct
    class type t = object
      method locator: (module Locator.S) Js.readonly_prop
    end
  end)(struct
    class type t = object
      method state: S.App.State.t Js.readonly_prop
    end
  end)

let handle_action ~locator:(module L:Locator.S) action =
  let module Reg = C.Command.Static_registry in
  match Reg.get L.command_registry ~name:action with
  | None -> ()
  | Some command ->
    let module C = C.Command.Static_command in
    command.C.executor [] L.context |> Lwt.ignore_result

let t = Component.make
    R.(component_spec
         ~constructor:(fun this _ ->
             let module L = (val this##.props##.locator : Locator.S) in
             S.App.Store.subscribe L.store ~f:(fun state ->
                 this##setState (object%js
                   val state = state
                 end)
               )
           )
         ~should_component_update:(fun _ _ _ -> true)
         (fun this ->
            let module L = (val this##.props##.locator : Locator.S) in
            let state = S.App.Store.get L.store in
            let keymap = S.Keymap.Store.get @@ S.App.State.keymap state in
            let condition = S.Config.State.condition @@ S.Config.Store.get @@ S.App.State.config state in
            [%c C_key_handler.t ~key:"key-container"
                ~keymap
                ~className:(Some "sf-Main")
                ~condition
                ~onAction:(handle_action ~locator:(module L))
                [
                  [%c C_omni_bar.t ~key:"omni-bar" ~locator:this##.props##.locator];
                  [%c C_layout.t ~key:"layout" ~locator:this##.props##.locator];
                ]]
         )
      )
