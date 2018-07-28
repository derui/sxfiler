(** {!C_viewer_stack} defines stack for viewer. *)
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
      method viewerStack: S.Viewer_stacks.State.t Js.readonly_prop
    end
  end)

let component = Component.make @@ fun props ->
  let module V = S.Viewer_stacks.State in
  R.create_element ~key:"file-tree"
    ~props:(object%js
      val locator = props##.locator
      val viewerState = (props##.viewerStack).V.file_tree
      val focused = true
    end)
    C_file_list_viewer.component
