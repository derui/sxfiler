module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method baseDirectory: Js.js_string Js.t Js.readonly_prop
      method fileName: Js.js_string Js.t Js.readonly_prop
      method directory: Js.js_string Js.t Js.readonly_prop
      method isDirectory: bool Js.readonly_prop
      method isSymbolicLink: bool Js.readonly_prop
      method marked: bool Js.readonly_prop
    end
  end)

let base_selector = "fp-FileItem_FileName"
let directory_modifier = base_selector ^ "-directory"
let symlink_modifier = base_selector ^ "-symlink"
let marked_modifier = base_selector ^ "-marked"

let get_classname props =
  let open Classnames in
  to_string @@ (empty
                <|> (base_selector, true)
                <|> (directory_modifier, props##.isDirectory)
                <|> (symlink_modifier, props##.isSymbolicLink)
                <|> (marked_modifier, props##.marked))

let resolve_name props =
  if props##.isDirectory then
    let base_dir = Js.to_string props##.baseDirectory
    and dir = Js.to_string props##.directory in
    String.sub dir (String.length base_dir) (String.length dir - String.length base_dir)
  else
    Js.to_string props##.fileName

let component = Component.make (fun props ->
    let name = resolve_name props in
    R.Dom.of_tag `span
      ~props:R.(element_spec ~class_name:(get_classname props) ())
      ~children:[R.text name]
  )
