module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.js Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let elements = Js.to_array props##.state##.file_list in
    let elements = Array.mapi (fun index item ->
        R.element ~key:(Js.to_string item##.uuid) ~props:(object%js
          val item = item
          val selected = Js.float_of_number props##.state##.selected_item
                         |> int_of_float
                         |> ( = ) index
                         |> Js.bool
        end) Sxfiler_file_item.component
      ) elements
    in
    R.Dom.of_tag `ul
      ~props:R.Core.Element_spec.({
          (empty ()) with class_name = Some (Sxfiler_classnames.make ["fp-FileList"])
        })
      ~children:elements
  )
