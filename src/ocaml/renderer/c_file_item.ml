module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method item: C.Types.File_stat.t Js.readonly_prop
      method selected: bool Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)
    (struct type t = unit end)

let component =
  let render this =
    let props = this##.props in
    let stat = props##.item in
    let class_name =
      let module C = Classnames in
      let v = C.(empty
                 <|> ("fp-FileItem", true)
                 <|> ("fp-FileItem-selected", props##.selected)
                 <|> ("fp-FileItem-focused", props##.focused && props##.selected))
      in C.to_string v
    in
    let module T = C.Types.File_stat in
    let file_mode = R.create_element ~key:"mode" ~props:(object%js
        val mode = stat.T.stat##.mode
      end) C_file_mode.component
    and timestamp = R.create_element ~key:"timestamp" ~props:(object%js
        val timestamp = stat.T.stat##.mtime
      end) C_file_timestamp.component
    and file_size = R.create_element ~key:"size" ~props:(object%js
        val size = stat.T.stat##.size
      end) C_file_size.component
    and file_name = R.create_element ~key:"name" ~props:(object%js
        val fileName = Js.string stat.T.filename
        val isDirectory = stat.T.stat##.isDirectory
        val isSymbolicLink = stat.T.stat##.isSymbolicLink
      end) C_file_name.component
    in
    R.Dom.of_tag `li
      ~props:R.(element_spec ~class_name ())
      ~children:[|file_mode; timestamp; file_size; file_name|]
  in
  let spec = R.component_spec
      ~should_component_update:(fun this new_props state -> this##.props <> new_props)
      render
  in
  Component.make spec
