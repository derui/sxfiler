module C = Sxfiler_common
module T = C.Types
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method pane: T.Pane.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let pane = props##.pane in
    let elements = List.mapi (fun index item ->
        let module F = C.Types.File_stat in
        R.element ~key:item.F.id ~props:(object%js
          val item = item
          val selected = pane.T.Pane.cursor_pos = index
        end) File_item.component
      ) pane.T.Pane.file_list
    in
    let children = Array.of_list elements in
    R.Dom.of_tag `ul
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(return "fp-FileList" |> to_string))
        })
      ~children
  )
