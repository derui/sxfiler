
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
    end
  end)

let spacer =
  let class_name = "fp-PaneLayout_Spacer" in
  R.Dom.of_tag `div ~key:"spacer" ~props:(R.element_spec ~class_name ())

let component = Component.make (fun props ->
    let state = props##.state in
    let module P = T.Pane in
    let create_pane_element key pane selected =
      R.create_element ~key ~props:(object%js
        val pane = pane
        val selected = selected
      end) C_file_list_pane.component
    in
    let state' = state.C.State.server_state in
    let left_pane = state'.C.Server_state.left_pane
    and right_pane = state'.C.Server_state.right_pane in
    let left_pane = create_pane_element "left" left_pane @@ C.Server_state.is_left_active state'
    and right_pane = create_pane_element "right" right_pane @@ C.Server_state.is_right_active state' in
    let children = [
      left_pane;
      spacer;
      right_pane;
    ]
    in
    R.Dom.of_tag `div ~props:R.(element_spec ~class_name:"fp-PaneLayout" ()) ~children
  )
