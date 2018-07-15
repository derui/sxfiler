(** {!C_viewer_stack} defines stack for viewer. *)
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

module Component = R.Component.Make_stateless(struct
    class type t = object
      method viewerStack: State.Viewer_stacks.t Js.readonly_prop
    end
  end)

let component = Component.make @@ fun props ->
  let module V = State.Viewer_stacks in
  R.create_element ~key:"file-tree"
    ~props:(object%js
      val viewerState = (props##.viewerStack).V.file_tree
      val focused = true
    end)
    C_file_list_viewer.component
