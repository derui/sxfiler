module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type _t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Sxfiler_key_dispatcher.t Js.readonly_prop
      method subscribe: (C.State.t -> unit) -> unit Js.meth
    end
    type t = _t Js.t
  end)(struct
    class type _t = object
      method state: C.State.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let key_handler props ev =
  let module D = Sxfiler_key_dispatcher in
  D.dispatch props##.dispatch ev

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this ->
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
              empty with class_name = Some (Sxfiler_classnames.make ["fp-KeyContainer"]);
                              on_key_down = Some (key_handler props);
                              on_key_up = Some (key_handler props);
                              on_key_press = Some (key_handler props);
                              others = Some (object%js
                                  val tabIndex = "0"
                                end);
            })
          ~children:([| R.element ~key:"file-list" ~props:(this##.state) Sxfiler_file_list.component
                     |])
      )

  }
