(** Viewer component provides a component to render some viewer.
    Supporting only of component is as container component.
*)
module T = Sxfiler_types

module R = Jsoo_reactjs

module Component = R.Component.Make_stateless(struct
    class type t = object
      method state: State.t Js.readonly_prop
      method viewerState: Types.Viewer_state.t Js.readonly_prop
    end
  end)

let component = Component.make @@ fun _ -> failwith "not implemented yet"
