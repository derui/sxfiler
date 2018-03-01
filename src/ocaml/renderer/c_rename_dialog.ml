module C = Sxfiler_common
module R = Jsoo_reactjs

module Original_name = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method name: Js.js_string Js.t Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      R.Dom.of_tag `div ~props:R.Core.Element_spec.({
          empty with
          class_name = Some Classnames.(return "dialog-RenameDialog_OriginalName" |> to_string)
        })
        ~children:[|
          R.text @@ Js.to_string props##.name
        |]
    )
end

module Input = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      R.Dom.of_tag `div ~props:R.Core.Element_spec.({
          empty with
          class_name = Some Classnames.(return "dialog-RenameDialog_InputContainer" |> to_string)
        })
        ~children:[|
          R.Dom.of_tag `input ~props:R.Core.Element_spec.({
              empty with class_name = Some "dialog-RenameDialog_Input"
            })
        |]
    )
end

module Button = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
        method selected: bool Js.t Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      let selected = Js.to_bool props##.selected in
      R.Dom.of_tag `div ~key:"yes" ~props:R.Core.Element_spec.({
          empty with class_name = Some (Classnames.(
          let open Infix in return "dialog-RenameDialog_Button"
                            <|> ("dialog-RenameDialog_Button-selected", selected)
                            |> to_string))
        })
        ~children:[|R.text @@ Js.to_string props##.text|];
    )
end

let text_container ~key ~children =
  let cls = Classnames.(return "dialog-RenameDialog_TextContainer" |> to_string) in
  R.Dom.of_tag `div ~key ~props:R.Core.Element_spec.({
      empty with class_name = Some cls
    })
    ~children

let key_handler ~dispatch ~this ev = failwith ""

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
        this##.state := object%js
          val filename = Js.string ""
        end
      );
    should_component_update = Some (fun this new_props state ->
        let should_update = this##.state##.filename <> state##.filename in
        should_update
      );
    render = (fun this ->
        let props = this##.props in
        let module T = C.Types.File_stat in
        let selected_file = C.State.(active_pane props##.state |> Pane.pointed_file) in
        let original = selected_file.T.filename in
        R.create_element C_dialog_base.component ~props:(object%js
          val title = Js.string "Rename object"
          val _open = Js.bool true
          val keyHandler = Js.Optdef.return (key_handler ~dispatch:props##.dispatch ~this);
        end) ~children:[|
          R.Dom.of_tag `div
            ~props:R.Core.Element_spec.({
                empty with class_name = Some (Classnames.(return "dialog-RenameDialog" |> to_string));
              })
            ~children:[|
              R.create_element ~key:"original" ~props:(object%js
                val name = Js.string original
              end) Original_name.component;

              R.create_element ~key:"input" ~props:(object%js
                val text = this##.state##.filename
              end) Input.component;

              text_container ~key:"text_container" ~children:[|
                R.text "Enter: execute; Esc: cancel"
              |]

            |]
        |]
      )

  }
