module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.js Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let elements = Js.to_array @@ Js.array_map (fun item ->
        R.element ~props:(object%js
          val item = item
        end)
          Sxfiler_file_item.component
      ) props##.state##.file_list
    in
    R.Dom.of_tag `ul
      ~props:R.Core.Element_spec.({
          (empty ()) with class_name = Some (Sxfiler_classnames.make ["fp-FileList"])
        })
      ~children:elements
  )
