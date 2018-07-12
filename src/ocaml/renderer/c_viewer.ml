(** Viewer component provides a component to render some viewer.
    Supporting only of component is as container component.
*)
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

module Component = R.Component.Make_stateless(struct
    class type t = object
      method viewerState: C.Types.Viewer_state.t Js.readonly_prop
    end
  end)

let component = Component.make @@ fun props ->
  let state = props##.viewerState in
  match state.C.Types.Viewer_state.viewer with
  | C.Types.Viewer.File_tree ft -> R.create_element ~key:"file-tree"
                                   ~props:(object%js
                                     val viewerState = ft
                                     val focused = true
                                   end)
                                   C_file_list_viewer.component
