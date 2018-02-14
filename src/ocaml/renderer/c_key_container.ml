module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

let key_handler ~dispatch ~state ev =
  ev##preventDefault;
  ev##stopPropagation;
  let key_map = state.C.State.config.C.Config.key_maps.C.Config.file_list in
  Key_dispatcher.dispatch dispatch ~state ~ev ~key_map

let component = Component.make {
    R.Core.Component_spec.empty with
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        let props = this##.props in
        R.Dom.of_tag `div
          ~props:R.Core.Element_spec.({
              empty with class_name = Some (Classnames.(return "sf-KeyContainer" |> to_string));
                         on_key_down = Some (key_handler ~dispatch:props##.dispatch ~state:(props##.state));
                         others = Some (object%js
                             val tabIndex = "0"
                           end);
            })
          ~children:[| R.element ~key:"file-list" ~props:(object%js
                         val state = props##.state
                       end) C_pane_layout.component |]
      )

  }
