(** {!C_completer_wrapper} provides container component to wrap input element and
    completer.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method completerId : string Js.readonly_prop

            method completion : S.Completion.State.t Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~initial_state:(fun _ _ ->
             object%js
               val showed = false
             end )
         ~initial_custom:(fun _ _ -> object%js end)
         ~component_will_receive_props:(fun this new_props ->
             let module S = S.Completion.State in
             let completion = new_props##.completion in
             let showed = Some new_props##.completerId = completion.S.current_completer in
             this##setState
               (object%js
                 val showed = showed
               end) )
         (fun this ->
            [%e
              div ~class_name:"sf-CompleterWrapper"
                [ [%e
                  div ~key:"input" ~class_name:"sf-CompleterWrapper_Input"
                    [R.Children.to_element this##.props_defined##.children]]
                ; [%e
                  div ~key:"completerWrapper" ~class_name:"sf-CompleterWrapper_PositionFixer"
                    [ [%c
                      P_completer.t ~key:"completer"
                        ~props:
                          (object%js
                            val completerId = this##.props##.completerId

                            val completion = this##.props##.completion

                            val showed = this##.state##.showed
                          end)] ]] ]] ))
