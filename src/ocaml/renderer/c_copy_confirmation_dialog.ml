module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method confirmed: bool Js.readonly_prop
    end
  end)

let key_handler ~dispatch ~state ev =
  ev##preventDefault;
  ev##stopPropagation;
  let key_map = state.C.State.config.C.Config.key_maps.C.Config.confirm_dialog in
  Key_dispatcher.dispatch dispatch ~state ~ev ~key_map

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
        R.element ~props:(object%js
          val title = Js.string "Confirmation to copy"
          val _open = Js.bool C.State.(state.dialog_state.Dialog.opening)
        end) ~children:[|
          R.Dom.of_tag `div
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "sf-ConfirmDialog" |> to_string));
                           on_key_down = Some (key_handler ~dispatch:props##.dispatch ~state:(this##.props##.state));
              })
            ~children:[|
              R.Dom.of_tag `span ~key:"yes" ~props:R.Core.Element_spec.({
                  empty with class_name = Some (Classnames.(
                  let open Infix in return "sf-ConfirmDialog_Confirm"
                                    <|> ("sf-ConfirmDialog_Confirm-Selected", this##.state##.confirmed)
                                    |> to_string))
                });
              R.Dom.of_tag `span ~key:"yes" ~props:R.Core.Element_spec.({
                  empty with class_name = Some (Classnames.(
                  let open Infix in return "sf-ConfirmDialog_Cancel"
                                    <|> ("sf-ConfirmDialog_Cancel-Selected", not this##.state##.confirmed)
                                    |> to_string))
                });
            |]

        |] C_dialog_base.component
      )
  }
