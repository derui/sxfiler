module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method file_name: Js.js_string Js.t Js.readonly_prop
      method isDirectory: bool Js.t Js.readonly_prop
      method isSymbolicLink: bool Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let base_selector = "fp-FileItem_FileName"
let directory_modifier = base_selector ^ "-Directory"

let get_classname props =
  let module C = Sxfiler_classnames in
  let open Sxfiler_classnames.Infix in
  ([base_selector] <|> (directory_modifier, Js.to_bool props##.isDirectory)) |> C.make

let component = Component.make (fun props ->
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = get_classname props
      end)
      ~children:[|
        R.text @@ Js.to_string props##.file_name
      |]
  )
