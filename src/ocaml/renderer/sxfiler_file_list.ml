module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let state = props##.state in
    let elements = List.mapi (fun index item ->
        let module T = C.Types.File_stat in
        R.element ~key:item.T.uuid ~props:(object%js
          val item = item
          val selected = (props##.state).selected_item = index
        end) Sxfiler_file_item.component
      ) state.file_list
    in
    let children = Array.of_list elements in
    R.Dom.of_tag `ul
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Sxfiler_classnames.make ["fp-FileList"])
        })
      ~children
  )
