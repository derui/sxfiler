module T = Sxfiler_types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method directory: string Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let directory = props##.directory in
    let class_name =
      let open Classnames in
      to_string ["fp-FileListPane_Header", true;
                 "fp-FileListPane_Header-focused", props##.focused]
    in
    R.Dom.of_tag `header
      ~props:R.(element_spec ~class_name ())
      ~children:[R.text directory]
  )
