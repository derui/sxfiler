module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method item: C.Types.File_stat.t Js.readonly_prop
      method selected: bool Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let stat = props##.item in
    let cls = let module C = Sxfiler_classnames in
      let open C.Infix in
      C.make (["fp-FileItem"] <|> ("is_selected", props##.selected |> Js.to_bool))
    in
    let module T = C.Types.File_stat in
    R.Dom.of_tag `li
      ~props:R.Core.Element_spec.({empty with class_name = Some (cls)})
      ~children:[|
        R.element ~key:"mode" ~props:(object%js val mode = stat.T.stat##.mode end) Sxfiler_file_mode.component;
        R.element ~key:"timestamp" ~props:(object%js val timestamp = stat.T.stat##.mtime end) Sxfiler_file_timestamp.component;
        R.element ~key:"size" ~props:(object%js val size = stat.T.stat##.size end) Sxfiler_file_size.component;
        R.element ~key:"name" ~props:(object%js
          val file_name = stat.T.filename
          val isDirectory = stat.T.stat##.isDirectory
          val isSymbolicLink = stat.T.stat##.isSymbolicLink
        end) Sxfiler_file_name.component;
      |]
  )
