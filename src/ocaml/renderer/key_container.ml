module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
      method subscribe: (C.State.t -> unit) -> unit Js.meth
    end
  end)(struct
    class type t = object
      method state: C.State.t Js.readonly_prop
    end
  end)

let key_handler ~dispatch ~state ev =
  ev##preventDefault;
  ev##stopPropagation;
  let key_map = state.C.State.config.C.Config.key_map in
  Key_dispatcher.dispatch dispatch ~state ~ev ~key_map

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        this##.props##subscribe (fun state ->
            this##setState (object%js
              val state = state
            end));

        this##.state := object%js
          val state = this##.props##.state
        end
      );
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        let props = this##.props in
        R.Dom.of_tag `div
          ~props:R.Core.Element_spec.({
              empty with class_name = Some (Classnames.(return "fp-KeyContainer" |> to_string));
                         on_key_down = Some (key_handler ~dispatch:props##.dispatch ~state:(this##.state##.state));
                         others = Some (object%js
                             val tabIndex = "0"
                           end);
            })
          ~children:[| R.element ~key:"file-list" ~props:(this##.state) Pane_layout.component |]
      )

  }
