module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method store: Store.t Js.readonly_prop
      method dispatch: Dispatcher.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method state: State.t Js.readonly_prop
    end
  end)

let component = Component.make
    R.(component_spec
         ~constructor:(fun this _ ->
             let store = this##.props##.store in
             Store.subscribe store (fun state ->
                        Firebug.console##log state;
                 this##setState (object%js
                   val state = state
                 end)
               );

             this##.state := object%js
               val state = store.Store.state
             end
           )
         ~should_component_update:(fun _ _ _ -> true)
         (fun this ->
            let props = this##.props in
            let children_props = object%js
              val dispatch = props##.dispatch
              val state = this##.state##.state
            end in
            R.Dom.of_tag `div
              ~props:R.(element_spec ~class_name:"sf-Main" ())
              ~children:[
                R.create_element ~key:"key-container" ~props:children_props C_key_container.component;
              ]
         )
      )
