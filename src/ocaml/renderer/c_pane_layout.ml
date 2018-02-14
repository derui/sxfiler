
module C = Sxfiler_common
module T = C.Types
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let state = props##.state in
    let children = Array.map (fun pane ->
        let module P = T.Pane in
        R.element ~key:(T.Pane_id.to_string pane.P.id) ~props:(object%js
          val pane = pane
          val selected = T.Pane_id.equal pane.T.Pane.id state.C.State.current_pane
        end) C_file_list_pane.component
      ) state.C.State.panes
    in
    let children = Array.concat [children;[|R.element ~key:"operations" ~props:(object%js
                                             val operationLog = state.C.State.operation_log
                                           end) C_operation_log_pane.component|]
                                ]
        in
    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(return "fp-PaneLayout" |> to_string))
        })
      ~children
  )
