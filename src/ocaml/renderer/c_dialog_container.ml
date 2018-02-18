module C = Sxfiler_common
module R = Reactjscaml
module T = C.Types
module M = C.Message

module Overlay = struct
  module C = R.Component.Make_stateless(struct
      class type t = object end
    end)

  let component = C.make (fun props ->
      let class_name = Classnames.(return "sf-DialogContainer_Overlay" |> to_string) in
      R.Dom.of_tag `div
        ~props:R.Core.Element_spec.({
            empty with class_name = Some class_name;
          })
    )
end

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
  | M.Operation.Copy _ -> Js.debugger ();R.element ~key:"dialog" ~props:(object%js
                            val state = props##.state
                            val dispatch = props##.dispatch
                            val title = Js.string "Confirm copy file"
                            val content = Js.string "Can filer copy file?"
                          end) C_confirmation_dialog.component

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
          val opened = C.State.(state.operation.Operation.confirming)
        end);
        true
      );
    should_component_update = Some (fun this _ _ -> true);
    render = (fun this ->
        let state = this##.props##.state in
        if not this##.state##.opened then R.empty ()
        else
          match C.State.(state.operation.Operation.next) with
          | None -> R.empty ()
          | Some typ -> begin
              Firebug.console##log typ;
              R.Dom.of_tag `div
                ~props:R.Core.Element_spec.({
                    empty with class_name = Some (Classnames.(return "sf-DialogContainer" |> to_string));
                  })
                ~children:[|
                  R.element ~key:"overlay" ~props:(object%js end) Overlay.component;
                  make_dialog ~props:this##.props typ
                |]
            end
      )

  }
