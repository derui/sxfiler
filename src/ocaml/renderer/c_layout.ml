(** Layout container for viewer stack. *)
open Sxfiler_core
module T = Sxfiler_types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless(struct
    class type t = object
      method state: State.t Js.readonly_prop
    end
  end)

(* [layout_container ~key ~state ~stack] *)
let layout_container ~key ~state stack  =
  let module C = T.Configuration in
  let class_name = match state.State.config.C.viewer.C.Viewer.stack_layout with
    | T.Types.Layout.Side_by_side -> Classnames.to_string [
        "fp-LayoutContainer", true;
        "fp-LayoutContainer_sideBySide", true;
      ]
  in
  R.Dom.of_tag `div
    ~props:R.(element_spec ~class_name ~key ())
    ~children:[R.create_element ~props:(object%js
                 val state = state
                 val viewerStack = stack
               end)
                 C_viewer_stack.component]

let component = Component.make @@ fun props ->
  let state = props##.state in
  let config = state.State.config in
  let module C = T.Configuration in
  let class_name = match config.C.viewer.C.Viewer.stack_layout with
    | T.Types.Layout.Side_by_side -> Classnames.to_string [
        "fp-Layout", true;
        "fp-Layout_sideBySide", true;
      ]
  in
  let to_container key =
    let open Util.Optdef in
    let container = Jstable.find state.State.viewer_stacks Js.(string key) >>= fun stack ->
      Js.Optdef.return @@ layout_container ~key ~state stack
    in
    Js.Optdef.to_option container |> Option.get_exn
  in

  R.Dom.of_tag `div
    ~props:R.(element_spec ~class_name ())
    ~children:(List.map to_container state.State.workspace_order)
