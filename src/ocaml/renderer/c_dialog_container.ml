module C = Sxfiler_common
module R = Reactjscaml
module T = C.Types

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
    end
  end)(struct
    class type t = object
      method opened: bool Js.readonly_prop
    end
  end)

let make_dialog ~props = function
  | T.Confirm_copy -> R.element ~props C_copy_confirmation_dialog.component
  | _ -> failwith "not implemented"

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        this##.state := object%js
          val opened = false
        end
      );
    component_will_receive_props = Some (fun this new_props ->
        let state = new_props##.state in
        this##setState (object%js
          val opened = C.State.(state.dialog_state.Dialog.opening)
        end);
        true
      );
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        let state = this##.props##.state in
        if not this##.state##.opened then R.empty ()
        else
          match C.State.(state.dialog_state.Dialog.typ) with
          | None -> R.empty ()
          | Some typ -> begin
              R.Dom.of_tag `div
                ~props:R.Core.Element_spec.({
                    empty with class_name = Some (Classnames.(return "sf-DialogContainer" |> to_string));
                  })
                ~children:[| make_dialog ~props:this##.props typ |]
            end
      )

  }
