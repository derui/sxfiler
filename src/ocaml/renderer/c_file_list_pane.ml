
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method pane: T.Pane.t Js.readonly_prop
      method selected: bool Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let pane = props##.pane in
    let class_name = Some Classnames.(
        let open Infix in
        return "fp-FileListPane" |> to_string
      )
    in
    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({empty with class_name})
      ~children:[|
        R.create_element ~key:"header" ~props:(object%js
          val directory = Js.string pane.T.Pane.directory
          val selected = props##.selected
        end) C_file_list_pane_header.component;
        R.Dom.of_tag `div ~key:"content"
          ~props:R.Core.Element_spec.({
              empty with class_name = Some (Classnames.(return "fp-FileListPane_Content" |> to_string))
            })
          ~children:[|
            R.create_element ~key:"file-list" ~props:(object%js
              val items = pane.T.Pane.file_list
              val cursor_pos = pane.T.Pane.cursor_pos
            end) C_file_list.component
          |]
      |]
  )
