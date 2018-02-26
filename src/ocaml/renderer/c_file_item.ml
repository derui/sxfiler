module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type t = object
      method item: C.Types.File_stat.t Js.readonly_prop
      method selected: bool Js.readonly_prop
    end
  end)
    (struct type t = unit end)

let component = Component.make {
    R.Core.Component_spec.empty with
    should_component_update = Some (fun this new_props state ->
        let module F = C.Types.File_stat in
        let should_update = (not @@ F.equal this##.props##.item new_props##.item) ||
                            (this##.props##.selected <> new_props##.selected) in
        should_update
      );
    render = (fun this ->
        let props = this##.props in
        let stat = props##.item in
        let cls = let module C = Classnames in
          let open C.Infix in
          C.(return "fp-FileItem" <|> ("is-selected", props##.selected) |> to_string)
        in
        let module T = C.Types.File_stat in
        R.Dom.of_tag `li
          ~props:R.Core.Element_spec.({empty with class_name = Some cls})
          ~children:[|
            R.element ~key:"mode" ~props:(object%js val mode = stat.T.stat##.mode end) C_file_mode.component;
            R.element ~key:"timestamp" ~props:(object%js val timestamp = stat.T.stat##.mtime end) C_file_timestamp.component;
            R.element ~key:"size" ~props:(object%js val size = stat.T.stat##.size end) C_file_size.component;
            R.element ~key:"name" ~props:(object%js
              val fileName = Js.string stat.T.filename
              val isDirectory = stat.T.stat##.isDirectory
              val isSymbolicLink = stat.T.stat##.isSymbolicLink
            end) C_file_name.component;
          |]
      )

  }
