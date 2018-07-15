module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: (module C.Context_intf.Instance) Js.readonly_prop
    end
  end)(struct
    class type t = object
      method state: C.Store_group.t Js.readonly_prop
    end
  end)

let component = Component.make
    R.(component_spec
         ~constructor:(fun this _ ->
             let module I = (val this##.props##.context : C.Context_intf.Instance) in
             I.Context.subscribe I.instance (`Change (fun state ->
                 this##setState (object%js
                   val state = state
                 end)
               ));
           )
         ~should_component_update:(fun _ _ _ -> true)
         (fun this ->
            let children_props = object%js
              val context = this##.props##.context
            end in
            R.Dom.of_tag `div
              ~props:R.(element_spec ~class_name:"sf-Main" ())
              ~children:[
                R.create_element ~key:"key-container" ~props:children_props C_key_container.component;
              ]
         )
      )
