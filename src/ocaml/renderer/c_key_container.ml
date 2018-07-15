module T = Sxfiler_types
module R = Jsoo_reactjs
module Be = Sxfiler_renderer_behavior

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: Context.t Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

let key_handler ~context ev =
  let module Behavior = Be.Default_key_handler in
  let module Core = Sxfiler_renderer_core in
  let config = Store.Config_store.get @@ Context.get_store context ~tag:Store.config in
  let module C = T.Configuration in
  let key_map = config.C.viewer.C.Viewer.key_maps.C.Key_maps.default in
  let behavior = Behavior.make key_map in
  Behavior.execute behavior ev

let other_props =
  Some (object%js
    val tabIndex = "0"
  end)

let container_key = "filePaneContainer"
let component =
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.nodes := Jstable.create ()
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
             ~on_key_down:(key_handler ~context:props##.context)
             ?others:other_props
         in
         R.Dom.of_tag `div
           ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:container_key ~value:e)
           ~props:spec
           ~children:[
             R.create_element ~key:"layout" ~props:(object%js
               val context = props##.context
             end) C_layout.component
           ]
      )
  in
  Component.make spec
