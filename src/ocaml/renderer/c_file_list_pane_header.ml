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
    R.Dom.of_tag `header
      ~props:R.Core.Element_spec.({
          empty with class_name = Some Classnames.(
          let open Infix in
          return "fp-FileListPane_Header"
          <|> ("fp-FileListPane_Header-selected", props##.selected)
          |> to_string)
        })
      ~children:[|R.text directory|]
  )
