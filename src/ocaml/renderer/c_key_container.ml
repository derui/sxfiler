module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Dispatcher.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method active: bool Js.readonly_prop
    end
  end)

let key_handler ~dispatch ~state ev =
  let key_map = state.C.State.config.C.Config.key_maps.C.Config.file_list in
  let dispatched = Key_handler.handle_key_event dispatch ~state ~ev ~key_map in
  if dispatched then begin
    ev##preventDefault; ev##stopPropagation
  end else ()

let is_active state =
  let module S = C.State in
  match Util.get_focus_target state.S.dialog_state with
  | Types.Focus_file_pane -> true
  | _ -> false

let other_props state =
  if is_active state then Some (object%js
      val tabIndex = "0"
    end)
  else None

let container_key = "filePaneContainer"
let component =
  let spec = R.component_spec
      ~constructor:(fun this props ->
          this##.nodes := Jstable.create ();
          this##.state := object%js
            val active = true
          end
        )
      ~component_did_update:(fun this _ _ ->
          match R.Ref_table.find this##.nodes ~key:container_key with
          | Some e -> if is_active (this##.props##.state) then e##focus else ()
          | None -> ()
        )
      ~component_will_receive_props:(fun this new_props ->
          this##setState (object%js val active = is_active new_props##.state end)
        )
      (fun this ->
         let props = this##.props in
         let spec = R.element_spec ()
             ~class_name:"sf-KeyContainer"
             ~on_key_down:(key_handler ~dispatch:props##.dispatch ~state:(props##.state))
             ?others:(other_props props##.state)
         in
         R.Dom.of_tag `div
           ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:container_key ~value:e)
           ~props:spec
           ~children:[
             R.create_element ~key:"file-list" ~props:(object%js
               val state = props##.state
             end) C_pane_layout.component
           ]
      )
  in
  Component.make spec
