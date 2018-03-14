module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method fileName: Js.js_string Js.t Js.readonly_prop
      method isDirectory: bool Js.t Js.readonly_prop
      method isSymbolicLink: bool Js.t Js.readonly_prop
    end
  end)

let base_selector = "fp-FileItem_FileName"
let directory_modifier = base_selector ^ "-Directory"
let symlink_modifier = base_selector ^ "-Symlink"

let get_classname props =
  let open Classnames in
  to_string @@ (empty
                <|> (base_selector, true)
                <|> (directory_modifier, Js.to_bool props##.isDirectory)
                <|> (symlink_modifier, Js.to_bool props##.isSymbolicLink))

let component = Component.make (fun props ->
    let name = Js.to_string props##.fileName in
    R.Dom.of_tag `span
      ~props:R.(element_spec ~class_name:(get_classname props) ())
      ~children:[| R.text name |]
  )
