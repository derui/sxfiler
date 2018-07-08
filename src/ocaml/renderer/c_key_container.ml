module T = Sxfiler_types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: State.t Js.readonly_prop
      method dispatch: Dispatcher.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method active: bool Js.readonly_prop
    end
  end)

let key_handler ~dispatch ~state ev =
  let module C = T.Configuration in
  let config = state.State.config in
  let key_map = config.C.viewer.C.Viewer.key_maps.C.Key_maps.file_list in
  let dispatched = Key_handler.handle_key_event dispatch ~ev ~key_map in
  if dispatched then begin
    ev##preventDefault; ev##stopPropagation
  end else ()

let other_props =
  Some (object%js
    val tabIndex = "0"
  end)

let container_key = "filePaneContainer"
let component =
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.nodes := Jstable.create ();
          this##.state := object%js
            val active = true
          end
        )
      ~component_did_update:(fun this _ _ ->
          match R.Ref_table.find this##.nodes ~key:container_key with
          | Some e -> e##focus
          | None -> ()
        )
      (fun this ->
         let props = this##.props in
         let spec = R.element_spec ()
             ~class_name:"sf-KeyContainer"
             ~on_key_down:(key_handler ~dispatch:props##.dispatch ~state:(props##.state))
             ?others:other_props
         in
         R.Dom.of_tag `div
           ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:container_key ~value:e)
           ~props:spec
           ~children:[
             R.create_element ~key:"layout" ~props:(object%js
               val state = props##.state
             end) C_layout.component
           ]
      )
  in
  Component.make spec
