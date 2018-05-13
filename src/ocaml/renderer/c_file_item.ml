module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method baseDirectory: Js.js_string Js.t Js.readonly_prop
      method item: C.Types.File_stat.t Js.readonly_prop
      method selected: bool Js.readonly_prop
      method focused: bool Js.readonly_prop
      method marked: bool Js.readonly_prop
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
        val mode = stat.T.mode
      end) C_file_mode.component
    and timestamp = R.create_element ~key:"timestamp" ~props:(object%js
        val timestamp = stat.T.mtime |> Int64.to_float
      end) C_file_timestamp.component
    and file_size = R.create_element ~key:"size" ~props:(object%js
        val size = stat.T.size
      end) C_file_size.component
    and file_name = R.create_element ~key:"name" ~props:(object%js
        val baseDirectory = props##.baseDirectory
        val fileName = Js.string stat.T.filename
        val directory = Js.string stat.T.directory
        val isDirectory = stat.T.is_directory
        val isSymbolicLink = stat.T.is_symlink
        val marked = props##.marked
      end) C_file_name.component
    in
    R.Dom.of_tag `li
      ~props:R.(element_spec ~class_name ())
      ~children:[file_mode; timestamp; file_size; file_name]
  in
  let spec = R.component_spec
      ~should_component_update:(fun this new_props state -> this##.props <> new_props)
      render
  in
  Component.make spec
