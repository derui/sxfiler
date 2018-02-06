
module C = Sxfiler_common
module T = C.Types
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let state = props##.state in
    let children = Array.map (fun pane ->
        let module P = T.Pane in
        R.element ~key:(T.Pane_location.Variants.to_name pane.P.location) ~props:(object%js
          val pane = pane
        end) File_list_pane.component
      ) state.C.State.panes
    in
    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(return "fp-PaneLayout" |> to_string))
        })
      ~children
  )
