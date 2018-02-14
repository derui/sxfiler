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
        let children_props = object%js
              val dispatch = props##.dispatch
              val state = this##.state##.state
            end in
        R.Dom.of_tag `div
          ~props:R.Core.Element_spec.({
              empty with class_name = Some (Classnames.(return "sf-Main" |> to_string));
            })
          ~children:[|
            R.element ~key:"key-container" ~props:children_props C_key_container.component;
            R.element ~key:"dialog-container" ~props:children_props C_dialog_container.component;
          |]
      )

  }
