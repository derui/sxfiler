module C = Sxfiler_common
module R = Reactjscaml

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
let symlink_modifier = base_selector ^ "-Symlink"

let get_classname props =
  let module C = Sxfiler_classnames in
  let open Sxfiler_classnames.Infix in
  ([base_selector]
   <|> (directory_modifier, Js.to_bool props##.isDirectory)
   <|> (symlink_modifier, Js.to_bool props##.isSymbolicLink)) |> C.make

let component = Component.make (fun props ->
    let name = props##.file_name |> Js.to_string |> Filename.basename in
    R.Dom.of_tag `span
      ~props:R.Core.Element_spec.({
        (empty ()) with class_name = Some (get_classname props)
      })
      ~children:[|
        R.text name
      |]
  )
