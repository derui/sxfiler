module T = Sxfiler_types
module R = Jsoo_reactjs
module L = Modules.Lodash

module Component = R.Component.Make_stateful (struct
    class type t = object
      method item: T.Node.t Js.readonly_prop
      method selected: bool Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)
    (struct type t = unit end)

let component =
  let render this =
    let props = this##.props in
    let node = props##.item in
    let stat = node.T.Node.stat in
    let class_name = Classnames.to_string
        ["fp-FileItem", true;
         "fp-FileItem-selected", props##.selected;
         "fp-FileItem-focused", props##.focused && props##.selected;
        ]
    in
    let module F = T.File_stat in
    let file_mode = R.create_element ~key:"mode" ~props:(object%js
        val mode = stat.F.mode
      end) P_file_mode.component
    and timestamp = R.create_element ~key:"timestamp" ~props:(object%js
        val timestamp = stat.F.mtime |> Int64.to_float
      end) P_file_timestamp.component
    and file_size = R.create_element ~key:"size" ~props:(object%js
        val size = stat.F.size
      end) P_file_size.component
    and file_name = R.create_element ~key:"name" ~props:(object%js
        val parentDirectory = node.T.Node.parent_directory
        val path = node.T.Node.full_path
        val isDirectory = stat.F.is_directory
        val isSymbolicLink = stat.F.is_symlink
      end) P_file_name.component
    in
    R.Dom.of_tag `li
      ~props:R.(element_spec ~class_name ())
      ~children:[file_mode; timestamp; file_size; file_name]
  in
  let spec = R.component_spec
      ~should_component_update:(fun this new_props _ -> not @@ L.isEqual this##.props new_props)
      render
  in
  Component.make spec
