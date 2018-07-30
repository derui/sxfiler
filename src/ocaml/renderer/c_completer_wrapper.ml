(** {!C_completer_wrapper} provides container component to wrap input element and
    completer.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful(struct
    class type t = object
      method completerId: string Js.readonly_prop
      method completion: S.Completion.State.t Js.readonly_prop
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)
    (struct
      class type t = object
        method showed: bool Js.readonly_prop
      end
    end)

let t =
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.state := object%js
            val showed = false
          end
        )
      ~should_component_update:(fun this props state ->
          let module S = S.Completion.State in
          let completion = props##.completion in
          Some this##.props##.completerId <> completion.S.current_completer
          || this##.state##.showed <> state##.showed
        )
      ~component_will_receive_props:(fun this new_props ->
          let module S = S.Completion.State in
          let completion = new_props##.completion in
          let showed = Some this##.props##.completerId <> completion.S.current_completer in
          this##setState (object%js
            val showed = showed
          end)
        )

      (fun this ->
         let children = [
           R.Dom.of_tag `div ~key:"input"
             ~props:(R.element_spec () ~class_name:"sf-CompleterWrapper_Input")
             ~children:[R.Children.to_element this##.props_defined##.children];
           R.Dom.of_tag `div ~key:"completer"
             ~props:(R.element_spec () ~class_name:"sf-CompleterWrapper_PositionFixer")
             ~children:[
               R.create_element
                 ~key:"completer"
                 ~props:(object%js
                   val completerId = this##.props##.completerId
                   val completion = this##.props##.completion
                   val showed = this##.state##.showed
                 end)
                 C_completer.t;
             ];
         ]
         in
         R.create_element ~key:"handler" ~props:(object%js
           val className = Some "sf-CompleterWrapper"
           val keymap = None
           val locator = this##.props##.locator
         end)
           ~children
           C_key_handler.t
      )
  in Component.make spec
