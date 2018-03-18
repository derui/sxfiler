
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
    let marked_items' = List.map snd pane.T.Pane.marked_items |> Array.of_list in
    let header = R.create_element ~key:"header" ~props:(object%js
        val directory = Js.string pane.T.Pane.directory
        val selected = props##.selected
      end) C_file_list_pane_header.component
    and content =
      let props = R.element_spec ~class_name:"fp-FileListPane_Content" ()
      and children = [|
        R.create_element ~key:"file-list" ~props:(object%js
          val items = pane.T.Pane.file_list
          val markedItems = marked_items'
          val focusedItem = pane.T.Pane.focused_item
          val focused = props##.selected
        end) C_file_list.component
      |]
      in
      R.Dom.of_tag `div ~key:"content" ~props ~children
    in
    let props = R.element_spec ~class_name:"fp-FileListPane" () in
    R.Dom.of_tag `div ~props ~children:[| header; content; |]
  )
