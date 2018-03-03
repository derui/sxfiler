
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
    R.Dom.of_tag `div
      ~props:R.(element_spec ~class_name:"fp-FileListPane"  ())
      ~children:[|
        R.create_element ~key:"header" ~props:(object%js
          val directory = Js.string pane.T.Pane.directory
          val selected = props##.selected
        end) C_file_list_pane_header.component;
        R.Dom.of_tag `div ~key:"content"
          ~props:R.(element_spec ~class_name:"fp-FileListPane_Content" ())
          ~children:[|
            R.create_element ~key:"file-list" ~props:(object%js
              val items = pane.T.Pane.file_list
              val cursor_pos = pane.T.Pane.cursor_pos
            end) C_file_list.component
          |]
      |]
  )
