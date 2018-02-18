module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
      method title: Js.js_string Js.t Js.readonly_prop
      method content: Js.js_string Js.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method confirmed: bool Js.readonly_prop
    end
  end)

(* Content component to show content of the dialog *)
module Content = struct
  module C = R.Component.Make_stateless(struct
    class type t = object
      method content: Js.js_string Js.t Js.readonly_prop
    end
  end)

  let component = C.make (fun props ->
    R.Dom.of_tag `div ~key:"content" ~props:R.Core.Element_spec.({
        empty with class_name = Some (Classnames.(return "sf-ConfirmDialog_Content" |> to_string))
      })
      ~children:[|R.text @@ Js.to_string props##.content|];
  )
end

module Button = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method selected: bool Js.t Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      let selected = Js.to_bool props##.selected in
      R.Dom.of_tag `div ~key:"yes" ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(
          let open Infix in return "sf-ConfirmDialog_Button"
                            <|> ("sf-ConfirmDialog_Button-Selected", selected)
                            |> to_string))
        });
    )
end

let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
let key_handler ~dispatch ~state ev =
  ev##preventDefault;
  ev##stopPropagation;

  let key = Util.keyboard_event_to_key ev |> Js.to_string in
  let module M = C.Message in
  match key with
  | _ when key = enter_key ->
    Key_dispatcher.dispatch ~dispatcher:dispatch ~message:M.request_confirm_operation
  | _ -> failwith ""

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        this##.state := object%js
          val confirmed = false
        end
      );
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        let props = this##.props in
        let state = props##.state in
        R.element C_dialog_base.component ~props:(object%js
          val title = props##.title
          val _open = Js.bool C.State.(state.dialog_state.Dialog.opening)
        end) ~children:[|
          R.Dom.of_tag `div
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "sf-ConfirmDialog" |> to_string));
                           on_key_down = Some (key_handler ~dispatch:props##.dispatch ~state:(this##.props##.state));
              })
            ~children:[|
              R.element ~key:"content" ~props:(object%js
                val content = props##.content
              end) Content.component;

              R.element ~key:"yes" ~props:(object%js
                val selected = Js.bool this##.state##.confirmed
              end) Button.component;

              R.element ~key:"no" ~props:(object%js
                val selected = Js.bool @@ not this##.state##.confirmed
              end) Button.component;
            |]
        |]
      )
  }
