module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful (struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)(struct
    class type t = object
      method state: S.App.State.t Js.readonly_prop
    end
  end)

let t = Component.make
    R.(component_spec
         ~constructor:(fun this _ ->
             let module L = (val this##.props##.locator : Locator.Main) in
             S.App.Store.subscribe L.store ~f:(fun state ->
                 this##setState (object%js
                   val state = state
                 end)
               )
           )
         ~should_component_update:(fun _ _ _ -> true)
         (fun this ->
            let module L = (val this##.props##.locator : Locator.Main) in
            let state = S.App.Store.get L.store in
            let keymap = S.Keymap.Store.get @@ S.App.State.keymap state in
            let condition = S.Config.State.condition @@ S.Config.Store.get @@ S.App.State.config state in
            [%e div ~class_name:"sf-Main" [
                [%c C_key_handler.t ~key:"key-container"
                    ~keymap
                    ~className:None
                    ~condition
                    ~onAction:(fun _ -> failwith "not implemented yet")
                    [
                      [%c C_omni_bar.t ~key:"omni-bar" ~locator:this##.props##.locator];
                      [%c C_layout.t ~key:"layout" ~locator:this##.props##.locator];
                    ]]]]
         )
      )
