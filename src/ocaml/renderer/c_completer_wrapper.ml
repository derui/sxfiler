(** {!C_completer_wrapper} provides container component to wrap input element and
    completer.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

type action =
  | Next
  | Prev
  | Select

let default_key_map =
  let bindings = List.map (fun (key, value) -> T.Key_map.{
      key = Sxfiler_kbd.to_keyseq key;
      action = value;
      condition = T.Condition.empty
    })
    [
      Sxfiler_kbd.make "ArrowUp", "prev";
      Sxfiler_kbd.make "ArrowDown", "next";
      Sxfiler_kbd.make "Enter", "select";
    ]
  in {T.Key_map.bindings}

let handle_action this = function
  | "prev" -> this##.props##.onPrev ()
  | "next" -> this##.props##.onNext ()
  | "select" -> begin
      match S.Completion.State.selected_item this##.props##.completion with
      | Some v -> this##.props##.onCompleted v
      | None -> ()
    end
  | _ -> ()

let t =
  R.Component.make_stateful
    ~props:(module struct
             class type t = object
               method completerId: string Js.readonly_prop
               method completion: S.Completion.State.t Js.readonly_prop
               method onCompleted: (T.Completion.Candidate.t -> unit) Js.readonly_prop
               method onNext: (unit -> unit) Js.readonly_prop
               method onPrev: (unit -> unit) Js.readonly_prop
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
                 Some this##.props##.completerId = completion.S.current_completer
                 || this##.state##.showed <> state##.showed
               )
             ~component_will_receive_props:(fun this new_props ->
                 let module S = S.Completion.State in
                 let completion = new_props##.completion in
                 let showed = Some new_props##.completerId = completion.S.current_completer in
                 this##setState (object%js
                   val showed = showed
                 end)
               )

             (fun this ->
                [%c C_key_handler.t
                    ~props:(object%js
                      val onAction = handle_action this
                      val className = Some "sf-CompleterWrapper"
                      val keymap = default_key_map
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
