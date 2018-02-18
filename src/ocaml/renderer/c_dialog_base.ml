module C = Sxfiler_common
module R = Reactjscaml
module T = C.Types

module Component = R.Component.Make_stateful (struct
    class type t = object
      method _open: bool Js.t Js.readonly_prop
      method title: Js.js_string Js.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method opened: bool Js.readonly_prop
    end
  end)

let header ~key ~title = R.Dom.of_tag `div ~key ~props:R.Core.Element_spec.({
    empty with class_name = Some (Classnames.(return "sf-DialogBase_Header" |> to_string))
  }) ~children:[|R.text title|]

let body ~key ~children = R.Dom.of_tag `div ~key ~props:R.Core.Element_spec.({
    empty with class_name = Some (Classnames.(return "sf-DialogBase_Body" |> to_string))
  }) ~children

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        this##.state := object%js
          val opened = Js.to_bool props##._open
        end
      );
    component_will_receive_props = Some (fun this new_props ->
        this##setState (object%js
          val opened = Js.to_bool new_props##._open
        end);
        true
      );
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        if not this##.state##.opened then R.empty ()
        else
          R.Dom.of_tag `div
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "sf-DialogBase" |> to_string));
              })
            ~children:[|
              header ~key:"header" ~title:(Js.to_string this##.props##.title);
              body ~key:"body" ~children:(Js.to_array this##.props_defined##.children)
            |]
      )
  }
