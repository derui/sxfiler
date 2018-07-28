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

let make_command_pallet locator =
  R.create_element ~key:"command-pallet"
    ~props:(object%js
      val locator = locator
    end)
    C_command_pallet.t

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
            let children_props = object%js
              val locator = this##.props##.locator
              val className = None
              val keymap = None
            end in
            let layout = R.create_element ~key:"layout" ~props:(object%js
                val locator = this##.props##.locator
              end) C_layout.component;
            in
            let children = [
              make_command_pallet this##.props##.locator;
              layout;
            ] in
            R.Dom.of_tag `div
              ~props:R.(element_spec ~class_name:"sf-Main" ())
              ~children:[
                R.create_element ~key:"key-container"
                  ~props:children_props
                  ~children
                  C_key_handler.t;
              ]
         )
      )
