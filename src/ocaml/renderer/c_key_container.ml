open Sxfiler_core
module T = Sxfiler_types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: Context.t Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

let handle_key_event: Dispatcher.t ->
  ev:R.Event.Keyboard_event.t ->
  key_map:T.Key_map.t -> bool = fun dispatch ~ev ~key_map ->
  let module K = T.Key_map in
  let module E = R.Event in
  let module KE = E.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = Util.keyboard_event_to_key ev in

      let open Option.Infix in
      let result = K.find key_map ~key
        >>= (fun action -> Some (dispatch @@ Action_creator.create action))
        >|= (fun () -> true)in
      Option.get ~default:false result
    end

let key_handler ~dispatch ~state ev =
  let module C = T.Configuration in
  let config = state.State.config in
  let key_map = config.C.viewer.C.Viewer.key_maps.C.Key_maps.file_list in
  let dispatched = handle_key_event dispatch ~ev ~key_map in
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
