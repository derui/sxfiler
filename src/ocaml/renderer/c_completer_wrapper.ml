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

let default_key_map =
  let module A = C.Callable_action in
  let keymap = C.Key_map.empty in
  let condition = C.Types.Condition.empty in
  List.fold_left (fun keymap (key, action) ->
      C.Key_map.add ~condition ~key ~action keymap
    )
    keymap
    [
      "ArrowUp", A.Completion (A.Completion.Prev_candidate);
      "ArrowDown", A.Completion (A.Completion.Next_candidate);
    ]

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
         [%c C_key_handler.t
             ~className:(Some "sf-CompleterWrapper")
             ~keymap:(Some default_key_map)
             ~locator:this##.props##.locator
             [
               [%e div ~key:"input" ~class_name:"sf-CompleterWrapper_Input"
                   [R.Children.to_element this##.props_defined##.children]];
               [%e div ~key:"completerWrapper" ~class_name:"sf-CompleterWrapper_PositionFixer"
                   [[%c C_completer.t ~key:"completer"
                       ~completerId:this##.props##.completerId
                       ~completion:this##.props##.completion
                       ~showed:this##.state##.showed]]
               ]
             ]
         ]
      )
  in Component.make spec
