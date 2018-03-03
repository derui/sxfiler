module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method directory: Js.js_string Js.t Js.readonly_prop
      method selected: bool Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let directory = Js.to_string props##.directory in
    let class_name =
      let open Classnames in
      let open Infix in
      return "fp-FileListPane_Header"
      <|> ("fp-FileListPane_Header-selected", props##.selected)
      |> to_string
    in
    R.Dom.of_tag `header
      ~props:R.(element_spec ~class_name ())
      ~children:[|R.text directory|]
  )
