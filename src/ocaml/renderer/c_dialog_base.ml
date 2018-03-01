module C = Sxfiler_common
module R = Jsoo_reactjs
module T = C.Types

let overlay ~key =
  let class_name = Classnames.(return "sf-DialogBase_Overlay" |> to_string) in
  R.Dom.of_tag `div ~key
    ~props:R.Core.Element_spec.({
        empty with class_name = Some class_name;
      })

module Component = R.Component.Make_stateful (struct
    class type t = object
      method _open: bool Js.t Js.readonly_prop
      method title: Js.js_string Js.t Js.readonly_prop
      method keyHandler: (R.Event.Keyboard_event.t -> unit) Js.optdef Js.readonly_prop
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

let container ~key ~children = R.Dom.of_tag `div ~key ~props:R.Core.Element_spec.({
    empty with class_name = Some (Classnames.(return "sf-DialogBase_ContentContainer" |> to_string))
  }) ~children

let key_handler ~this ev =
  ev##preventDefault;
  ev##stopPropagation;

  match Js.Optdef.to_option this##.props##.keyHandler with
  | None -> ()
  | Some handler -> handler ev

let container_key = "container"
let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        this##.nodes := Jstable.create ();
        this##.state := object%js
          val opened = Js.to_bool props##._open
        end
      );
    component_will_receive_props = Some (fun this new_props ->
        this##setState (object%js
          val opened = Js.to_bool new_props##._open
        end)
      );
    should_component_update = Some (fun this _ _ -> true);
    component_did_mount = Some (fun this ->
        match R.Ref_table.find this##.nodes ~key:container_key with
        | None -> ()
        | Some e -> e##focus
      );
    component_will_unmount = Some (fun this -> R.Ref_table.remove this##.nodes ~key:container_key);
    render = (fun this ->
        if not this##.state##.opened then R.empty ()
        else
          R.Dom.of_tag `div
            ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:container_key ~value:e)
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "sf-DialogBase" |> to_string));
                           on_key_down = Some (key_handler ~this);
                           others = Some (object%js
                               val tabIndex = "0"
                             end)
              })
            ~children:[|
              overlay ~key:"overlay";
              container ~key:"container" ~children:[|
                header ~key:"header" ~title:(Js.to_string this##.props##.title);
                body ~key:"body" ~children:(Js.to_array this##.props_defined##.children)
              |]
            |]
      )
  }
