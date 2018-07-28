(** Layout container for viewer stack. *)
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)

let layout_container ~key locator stack  =
  let module C = T.Configuration in
  R.create_element ~key ~props:(object%js
    val locator = locator
    val viewerStack = stack
  end)
    C_viewer_stack.component

let component = Component.make @@ fun props ->
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

  R.Dom.of_tag `div
    ~props:R.(element_spec ~key:"layout" ~class_name ())
    ~children:[layout_container ~key:"stack" props##.locator viewer_stacks']
