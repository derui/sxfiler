(** {!C_completer_wrapper} provides container component to wrap input element and
    completer.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

type action =
  | Next
  | Prev
  | Select

module Component = R.Component.Make_stateful(struct
    class type t = object
      method completerId: string Js.readonly_prop
      method completion: S.Completion.State.t Js.readonly_prop
      method locator: (module Locator.Main) Js.readonly_prop
      method onCompleted: (T.Completion.Item.t -> unit) Js.readonly_prop
    end
  end)
    (struct
      class type t = object
        method showed: bool Js.readonly_prop
      end
    end)

let default_key_map =
  let module A = C.Callable_action in
  let keymap = T.Key_map.empty in
  let condition = T.Condition.empty in
  List.fold_left (fun keymap (key, value) -> T.Key_map.add ~condition ~key ~value keymap)
    keymap
    [
      {Sxfiler_kbd.empty with key = "ArrowUp"}, "prev";
      {Sxfiler_kbd.empty with key = "ArrowDown"}, "next";
      {Sxfiler_kbd.empty with key = "Enter"}, "select";
    ]

let handle_action = function
  | "prev" -> failwith "not implemented yet"
  | "next" -> failwith "not implemented yet"
  | "select" -> failwith "not implemented yet"
  | _ -> ()

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
         let module L = (val this##.props##.locator: Locator.Main) in
         let condition = S.Config.State.condition @@ S.Config.Store.get @@ S.App.(State.config @@ Store.get L.store)
         in
         [%c C_key_handler.t
             ~onAction:handle_action
             ~className:(Some "sf-CompleterWrapper")
             ~keymap:default_key_map
             ~condition
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
