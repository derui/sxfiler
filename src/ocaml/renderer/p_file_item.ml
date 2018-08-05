open Sxfiler_core
module T = Sxfiler_domain
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

let t =
  let render this =
    let props = this##.props in
    let node = props##.item in
    let stat = node.T.Node.stat in
    let module F = T.File_stat in
    let file_mode = [%c P_file_mode.t ~key:"mode" ~mode:stat.F.mode]
    and timestamp = [%c P_file_timestamp.t ~key:"timestamp" ~timestamp:(stat.F.mtime |> Int64.to_float)]
    and file_size = [%c P_file_size.t ~key:"size" ~size:stat.F.size]
    and file_name = [%c P_file_name.t ~key:"name"
        ~parentDirectory:node.T.Node.parent_directory
        ~path:(Path.to_string node.T.Node.full_path)
        ~isDirectory:stat.F.is_directory
        ~isSymbolicLink:stat.F.is_symlink
    ] in
    let class_name = Classnames.to_string
        ["fp-FileItem", true;
         "fp-FileItem-selected", props##.selected;
         "fp-FileItem-focused", props##.focused && props##.selected;
        ]
    in
    [%e li ~class_name [file_mode; timestamp; file_size; file_name]]
  in
  let spec = R.component_spec
      ~should_component_update:(fun this new_props _ -> not @@ L.isEqual this##.props new_props)
      render
  in
  Component.make spec
