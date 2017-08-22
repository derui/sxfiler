module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method file_name: Js.js_string Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = Sxfiler_classnames.make ["file-list__file-name"]
      end)
      ~children:[|
        R.text @@ Js.to_string props##.file_name
      |]
  )
