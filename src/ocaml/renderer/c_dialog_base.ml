module C = Sxfiler_common
module R = Jsoo_reactjs
module T = C.Types


let overlay ~key =
  R.Dom.of_tag `div ~key
    ~props:(R.element_spec ~class_name:"sf-DialogBase_Overlay" ())

let container ~key ~children =
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name:"sf-DialogBase_ContentContainer" ()) ~children

let key_handler ~this ev =
  match Js.Optdef.to_option this##.props##.keyHandler with
  | None -> ()
  | Some handler -> handler ev

module Component = R.Component.Make_stateful (struct
    class type t = object
      method _open: bool Js.t Js.readonly_prop
      method keyHandler: (R.Event.Keyboard_event.t -> unit) Js.optdef Js.readonly_prop
    end
  end)(struct
    class type t = object
      method opened: bool Js.readonly_prop
    end
  end)

let container_key = "container"
let component =
  let render this =
    if not this##.state##.opened then R.empty ()
    else R.Dom.of_tag `div
        ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:container_key ~value:e)
        ~props:R.(element_spec ()
                    ~class_name:"sf-DialogBase"
                    ~on_key_down:(key_handler ~this)
                    ~others:(object%js
                      val tabIndex = "0"
                    end))
        ~children:[|
          overlay ~key:"overlay";
          container ~key:"container" ~children:Js.(to_array this##.props_defined##.children);
        |]
  in
  Component.make
    R.(component_spec
         ~constructor:(fun this props ->
             this##.nodes := Jstable.create ();
             this##.state := object%js
               val opened = Js.to_bool props##._open
             end)
         ~component_will_receive_props:(fun this new_props ->
             this##setState (object%js
               val opened = Js.to_bool new_props##._open
             end))
         ~should_component_update:(fun this _ _ -> true)
         ~component_did_mount:(fun this ->
             match R.Ref_table.find this##.nodes ~key:container_key with
             | None -> ()
             | Some e -> e##focus)
         ~component_will_unmount:(fun this -> R.Ref_table.remove this##.nodes ~key:container_key)
         render
      )
