module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let handle_action ~locator:(module L : Locator.S) action =
  let module Command = Sxfiler_renderer_command in
  let module Reg = Command.Static_registry in
  match Reg.get L.command_registry ~name:action with
  | None -> ()
  | Some command ->
    let module C = Command.Static_command_runner in
    let state = S.App.Store.get L.store in
    C.run ~param:[] ~context:L.context ~state command |> Lwt.ignore_result

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method locator : (module Locator.S) Js.readonly_prop
          end
      end )
    ~spec:
      R.(
        component_spec
          ~initial_custom:(fun _ _ -> object%js end)
          ~initial_state:(fun _ props ->
              let module L = (val props##.locator : Locator.S) in
              object%js
                val state = S.App.Store.get L.store
              end )
          ~constructor:(fun this _ ->
              let module L = (val this##.props##.locator : Locator.S) in
              S.App.Store.subscribe L.store ~f:(fun state ->
                  this##setState
                    (object%js
                      val state = state
                    end) ) )
          ~should_component_update:(fun _ _ _ -> true)
          (fun this ->
             let module L = (val this##.props##.locator : Locator.S) in
             let state = S.App.Store.get L.store in
             let keymap = S.Keymap.Store.get @@ S.App.State.keymap state in
             [%c
               C_key_handler.t ~key:"key-container"
                 ~props:
                   (object%js
                     val keymap = keymap

                     val className = Some "sf-Main"

                     val onAction = handle_action ~locator:(module L)
                   end)
                 [ [%c
                   C_mode_manager.t ~key:"mode-manager"
                     ~props:
                       (object%js
                         val locator = this##.props##.locator
                       end)]
                 ; [%c
                   C_omni_bar.t ~key:"omni-bar"
                     ~props:
                       (object%js
                         val locator = this##.props##.locator
                       end)]
                 ; [%c
                   C_workspace.t ~key:"layout"
                     ~props:
                       (object%js
                         val locator = this##.props##.locator
                       end)] ]] ))
