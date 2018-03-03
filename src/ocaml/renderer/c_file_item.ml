module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method item: C.Types.File_stat.t Js.readonly_prop
      method selected: bool Js.readonly_prop
    end
  end)
    (struct type t = unit end)

let component =
  let render this =
    let props = this##.props in
    let stat = props##.item in
    let class_name = let module C = Classnames in
      let open C.Infix in
      C.(return "fp-FileItem" <|> ("is-selected", props##.selected) |> to_string)
    in
    let module T = C.Types.File_stat in
    R.Dom.of_tag `li
      ~props:R.(element_spec ~class_name ())
      ~children:[|
        R.create_element ~key:"mode" ~props:(object%js val mode = stat.T.stat##.mode end) C_file_mode.component;
        R.create_element ~key:"timestamp" ~props:(object%js val timestamp = stat.T.stat##.mtime end) C_file_timestamp.component;
        R.create_element ~key:"size" ~props:(object%js val size = stat.T.stat##.size end) C_file_size.component;
        R.create_element ~key:"name" ~props:(object%js
          val fileName = Js.string stat.T.filename
          val isDirectory = stat.T.stat##.isDirectory
          val isSymbolicLink = stat.T.stat##.isSymbolicLink
        end) C_file_name.component;
      |]
  in
  let spec = R.component_spec
      ~should_component_update:(fun this new_props state ->
          let module F = C.Types.File_stat in
          let should_update = (not @@ F.equal this##.props##.item new_props##.item) ||
                              (this##.props##.selected <> new_props##.selected) in
          should_update)
      render
  in
  Component.make spec
