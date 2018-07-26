module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)(struct
    class type t = object
      method state: S.App.Store.t Js.readonly_prop
    end
  end)

let make_command_pallet context =
  R.create_element ~key:"command-pallet"
    ~props:(object%js
      val context = context
    end)
    C_command_pallet.component

let component = Component.make
    R.(component_spec
         ~constructor:(fun this _ ->
             let module I = (val this##.props##.context : Context.Instance) in
             I.Context.subscribe I.instance (fun state ->
                 this##setState (object%js
                   val state = state
                 end)
               )
           )
         ~should_component_update:(fun _ _ _ -> true)
         (fun this ->
            let children_props = object%js
              val context = this##.props##.context
            end in
            let layout = R.create_element ~key:"layout" ~props:(object%js
                val context = this##.props##.context
              end) C_layout.component;
            in
            let children = [
              make_command_pallet this##.props##.context;
              layout;
            ] in
            R.Dom.of_tag `div
              ~props:R.(element_spec ~class_name:"sf-Main" ())
              ~children:[
                R.create_element ~key:"key-container"
                  ~props:children_props
                  ~children
                  C_key_handler.component;
              ]
         )
      )
