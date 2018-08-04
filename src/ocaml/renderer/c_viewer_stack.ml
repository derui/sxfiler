(** {!C_viewer_stack} defines stack for viewer. *)
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
      method viewerStack: S.Viewer_stacks.State.t Js.readonly_prop
    end
  end)

let t = Component.make @@ fun props ->
  let module V = S.Viewer_stacks.State in
  [%c C_file_list_viewer.t ~locator:props##.locator ~viewerState:(props##.viewerStack).V.file_tree
      ~focused:true]
