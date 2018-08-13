(** {!C_completer_wrapper} provides container component to wrap input element and
    completer.

    This component will appear beside of a base component that is passed from props.
*)

module D = Sxfiler_domain
module T = Sxfiler_completion.Domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

type action =
  | Next
  | Prev
  | Select

let default_key_map =
  let keymap = D.Key_map.make () in
  let condition = D.Condition.empty in
  List.fold_left (fun keymap (key, value) -> D.Key_map.add ~condition ~key ~value keymap)
    keymap
    [
      Sxfiler_kbd.make "ArrowUp", "prev";
      Sxfiler_kbd.make "ArrowDown", "next";
      Sxfiler_kbd.make "Enter", "select";
    ]

let handle_action = function
  | "prev" -> failwith "not implemented yet"
  | "next" -> failwith "not implemented yet"
  | "select" -> failwith "not implemented yet"
  | _ -> ()

let t =
  R.Component.make_stateful
    ~props:(module struct
             class type t = object
               method completerId: string Js.readonly_prop
               method completion: S.Completion.State.t Js.readonly_prop
               method locator: (module Locator.S) Js.readonly_prop
               method onCompleted: (T.Item.t -> unit) Js.readonly_prop
             end
           end)
    ~spec:(R.component_spec
             ~initial_state:(fun _ _ ->
                 object%js
                   val showed = false
                 end
               )
             ~initial_custom:(fun _ _ -> object%js end)
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
                let module L = (val this##.props##.locator: Locator.S) in
                let condition = S.Workspace.State.condition @@ S.Workspace.Store.get
                  @@ S.App.(State.workspace @@ Store.get L.store)
                in
                [%c C_key_handler.t
                    ~props:(object%js
                      val onAction = handle_action
                      val className = Some "sf-CompleterWrapper"
                      val keymap = default_key_map
                      val condition = condition
                    end)
                    [
                      [%e div ~key:"input" ~class_name:"sf-CompleterWrapper_Input"
                          [R.Children.to_element this##.props_defined##.children]];
                      [%e div ~key:"completerWrapper" ~class_name:"sf-CompleterWrapper_PositionFixer"
                          [[%c C_completer.t ~key:"completer"
                              ~props:(object%js
                                val completerId = this##.props##.completerId
                                val completion = this##.props##.completion
                                val showed = this##.state##.showed
                              end)
                          ]]
                      ]
                    ]
                ]
             )
          )
