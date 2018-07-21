module T = Sxfiler_types
module R = Jsoo_reactjs
module Be = Sxfiler_renderer_behavior
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

let key_handler ~context ev =
  let module I = (val context: Context.Instance) in
  let app = S.App.Store.get @@ I.Context.get_store I.instance in
  let keymap = S.Keymap.Store.get @@ S.App.State.keymap app in
  let condition = S.Config.State.condition @@ S.Config.Store.get @@ S.App.State.config app in
  I.Context.execute I.instance (module Be.Handle_key_event) (keymap, condition, ev)

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
