(** {!C_viewer_stack} defines stack for viewer. *)
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

module Component = R.Component.Make_stateless(struct
    class type t = object
      method viewerStack: C.Types.Viewer_stack.t Js.readonly_prop
    end
  end)

let component = Component.make @@ fun props ->
  R.Dom.of_tag `div
    ~props:R.(element_spec ~class_name:"fp-ViewerStack" ())
    ~children:(List.map (fun state -> R.create_element ~props:(object%js
                            val viewerState = state
                          end) C_viewer.component)
                 props##.viewerStack)
