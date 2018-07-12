(** Layout container for viewer stack. *)
open Sxfiler_core
module T = Sxfiler_types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless(struct
    class type t = object
      method context: Context.t Js.readonly_prop
    end
  end)

let layout_container ~key stack  =
  let module C = T.Configuration in
  let class_name = Classnames.to_string [
      "fp-LayoutContainer", true;
    ]
  in
  R.Dom.of_tag `div
    ~key
    ~props:R.(element_spec ~class_name ())
    ~children:[R.create_element ~key:"viewer-stack" ~props:(object%js
                 val viewerStack = stack
               end)
                 C_viewer_stack.component]

let component = Component.make @@ fun props ->
  let config = Context.get_store props##.context ~tag:Store.config in
  let config' = Store.Config_store.get config in
  let layout = Context.get_store props##.context ~tag:Store.layout in
  let layout' = Store.Layout_store.get layout in
  let viewer_stacks = Context.get_store props##.context ~tag:Store.viewer_stacks in
  let viewer_stacks' = Store.Viewer_stacks_store.get viewer_stacks in
  let module C = T.Configuration in
  let class_name = match config'.C.viewer.C.Viewer.stack_layout with
    | T.Types.Layout.Side_by_side -> Classnames.to_string [
        "fp-Layout", true;
        "fp-Layout_sideBySide", true;
      ]
  in
  let to_container key =
    let open Option.Infix in
    State.Viewer_stacks.find_by_name viewer_stacks' ~name:key >|= fun stack ->
    layout_container ~key stack
  in

  R.Dom.of_tag `div
    ~props:R.(element_spec ~key:"layout" ~class_name ())
    ~children:(List.map to_container layout'
               |> List.filter Option.is_some
               |> List.map Option.get_exn)
