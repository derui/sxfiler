module C = Sxfiler_common
module R = Jsoo_reactjs

module Input = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
        method onKeyDown: (R.Event.Keyboard_event.t -> unit) Js.readonly_prop
        method onInput: (R.Event.Input_event.t -> unit) Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      R.Dom.of_tag `div ~props:R.Core.Element_spec.({
          empty with
          class_name = Some Classnames.(return "dialog-RenameDialog_InputContainer" |> to_string)
        })
        ~children:[|
          R.Dom.of_tag `span ~props:R.Element_spec.({
              empty with class_name = Some "dialog_RenameDialog_InputLabel"
            })
            ~children:[| R.text "Name:" |];
          R.Dom.of_tag `input ~props:R.Core.Element_spec.({
              empty with class_name = Some "dialog-RenameDialog_Input";
                         on_key_down = Some props##.onKeyDown;
                         on_input = Some props##.onInput;
                         default_value = Some (Js.to_string props##.text);
                         others = Some (object%js
                           val tabIndex = "1"
                         end)
            })
        |]
    )
end

let text_container ~key ~children =
  let cls = Classnames.(return "dialog-RenameDialog_TextContainer" |> to_string) in
  R.Dom.of_tag `div ~key ~props:R.Core.Element_spec.({
      empty with class_name = Some cls
    })
    ~children

let key_handler ~dispatch ~this ev = ()

let input_handler ~dispatch ~this ev =
  let value = ev##.target##getAttribute (Js.string "value") in
  this##setState (object%js
    val filename = Js.Opt.get value (fun () -> Js.string "")
  end)

module Component = R.Component.Make_stateful
    (struct
      class type t = object
        method dispatch : Key_dispatcher.t Js.readonly_prop
        method onComplete : (unit -> C.Types.User_action.t) Js.readonly_prop
        method state : C.State.t Js.readonly_prop
      end
    end)
    (struct
      class type t = object
        method filename : Js.js_string Js.t Js.readonly_prop
      end
    end)

let component = Component.make {
    R.Core.Component_spec.empty with
    initialize = Some (fun this props ->
        let module T = C.Types.File_stat in
        let selected_file = C.State.(active_pane props##.state |> Pane.pointed_file) in
        let original = selected_file.T.filename in
        this##.state := object%js
          val filename = Js.string original
        end
      );
    should_component_update = Some (fun this new_props state ->
        let should_update = this##.state##.filename <> state##.filename in
        should_update
      );
    render = (fun this ->
        let props = this##.props in
        R.create_element C_dialog_base.component ~props:(object%js
          val title = Js.string "Rename object"
          val _open = Js.bool true
          val keyHandler = Js.Optdef.empty
        end) ~children:[|
          R.Dom.of_tag `div
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "dialog-RenameDialog" |> to_string));
              })
            ~children:[|
              R.create_element ~key:"input" ~props:(object%js
                val text = this##.state##.filename
                val onKeyDown = key_handler ~dispatch:props##.dispatch ~this
                val onInput = input_handler ~dispatch:props##.dispatch ~this
              end) Input.component;

              text_container ~key:"text_container" ~children:[|
                R.text "Enter: execute; Esc: cancel"
              |]

            |]
        |]
      )

  }
