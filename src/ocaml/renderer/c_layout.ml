(** Layout container for viewer stack. *)
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)

let layout_container ~key locator stack  =
  [%c C_viewer_stack.t ~key ~locator ~viewerStack:stack]

let t = Component.make @@ fun props ->
  let module L = (val props##.locator : Locator.Main) in
  let store = L.store in
  let config' = S.Config.Store.get @@ S.App.(State.config @@ Store.get store) in
  let viewer_stacks' = S.Viewer_stacks.Store.get @@ S.App.(State.viewer_stacks @@ Store.get store) in
  let module C = T.Configuration in
  let class_name = match (config'.S.Config.State.config).C.viewer.C.Viewer.stack_layout with
    | T.Types.Layout.Side_by_side -> Classnames.to_string [
        "fp-Layout", true;
        "fp-Layout_sideBySide", true;
      ]
  in

  [%e div ~key:"layout" ~class_name [
      layout_container ~key:"stack" props##.locator viewer_stacks';
    ]]
