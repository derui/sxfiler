
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let state = props##.state in
    let module P = T.Pane in
    let create_pane_element key pane selected =
      R.create_element ~key ~props:(object%js
        val pane = pane
        val selected = selected
      end) C_file_list_pane.component
    in
    let left_pane = state.C.State.left_pane
    and right_pane = state.C.State.right_pane in
    let left_pane = create_pane_element "left" left_pane @@ C.State.is_left_active state
    and right_pane = create_pane_element "right" right_pane @@ C.State.is_right_active state in
    let children = [|
      left_pane;
      right_pane;
      R.create_element ~key:"operations" ~props:(object%js
        val operationLog = state.C.State.operation_log
      end) C_operation_log_pane.component
    |]
    in
    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(return "fp-PaneLayout" |> to_string))
        })
      ~children
  )
