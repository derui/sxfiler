
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

module Component = R.Component.Make_stateless (struct
    class type t = object
      method viewerState: C.Types.Viewer.File_tree.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let state = props##.viewerState in
    let snapshot = state.C.Types.Viewer.File_tree.snapshot in
    let header = R.create_element ~key:"header" ~props:(object%js
        val directory = snapshot.T.Tree_snapshot.directory
        val focused = props##.focused
      end) C_file_list_viewer_header.component
    and content =
      let props = R.element_spec ~class_name:"fp-FileListViewer_Content" ()
      and children = [
        R.create_element ~key:"file-list" ~props:(object%js
          val viewerState = state
          val focused = props##.focused
        end) C_file_list.component
      ]
      in
      R.Dom.of_tag `div ~key:"content" ~props ~children
    in
    let props = R.element_spec ~class_name:"fp-FileListViewer" () in
    R.Dom.of_tag `div ~props ~children:[header; content]
  )
