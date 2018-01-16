module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method item: C.Types.File_stat.js Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let stat = props##.item in
    R.Dom.of_tag `li
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = Sxfiler_classnames.make ["fp-FileItem"]
      end)
      ~children:[|
        R.element ~props:(object%js val mode = stat##.stat##.mode end) Sxfiler_file_mode.component;
        R.element ~props:(object%js val timestamp = stat##.stat##.mtime end) Sxfiler_file_timestamp.component;
        R.element ~props:(object%js val size = stat##.stat##.size end) Sxfiler_file_size.component;
        R.element ~props:(object%js
          val file_name = stat##.filename
          val isDirectory = stat##.stat##.isDirectory
        end) Sxfiler_file_name.component;
      |]
  )
