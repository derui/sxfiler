module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
      method title: Js.js_string Js.t Js.readonly_prop
      method content: Js.js_string Js.t Js.readonly_prop
      method onComplete: (unit -> C.Types.User_action.t) Js.readonly_prop
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
      R.Dom.of_tag `div ~key:"content" ~props:R.(element_spec ~class_name:"dialog-ConfirmDialog_Content" ())
        ~children:[|R.text @@ Js.to_string props##.content|];
    )
end

let header ~key ~title =
  R.Dom.of_tag `div ~key
    ~props:R.(element_spec ~class_name:"dialog-ConfirmDialog_Header" ())
    ~children:[|R.text title|]

let body ~key ~children =
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name:"dialog-ConfirmDialog_Body" ()) ~children

module Button = struct
  module C = R.Component.Make_stateless(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
        method selected: bool Js.t Js.readonly_prop
      end
    end)

  let component = C.make (fun props ->
      let selected = Js.to_bool props##.selected in
      let class_name = Classnames.(
          let open Infix in
          return "dialog-ConfirmDialog_Button"
          <|> ("dialog-ConfirmDialog_Button-Selected", selected)
          |> to_string) in
      R.Dom.of_tag `div ~key:"yes" ~props:R.(element_spec ~class_name ())
        ~children:[|R.text @@ Js.to_string props##.text|];
    )
end

let button_container ~key ~children =
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name:"dialog-ConfirmDialog_ButtonContainer" ())
    ~children

let esc = Sxfiler_kbd.(to_keyseq {empty with key = "Escape"})
let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
let tab = Sxfiler_kbd.(to_keyseq {empty with key = "Tab"})
let key_handler ~dispatch ~this ev =
  let stop_default_behavior ev =
    ev##preventDefault;
    ev##stopPropagation
  in
  let key = Util.keyboard_event_to_key ev |> Js.to_string in
  let module M = C.Message in
  match key with
  | _ when key = enter_key -> begin
      stop_default_behavior ev;

      let message =
        if this##.state##.confirmed then
          let action = this##.props##.onComplete () |> C.Types.User_action.to_js in
          M.close_dialog action
        else
          M.close_dialog @@ C.Types.User_action.(to_js Cancel)
      in
      Key_dispatcher.dispatch ~dispatcher:dispatch ~message
    end
  | _ when key = esc -> begin
      stop_default_behavior ev;

      let message = M.close_dialog @@ C.Types.User_action.(to_js Cancel) in
      Key_dispatcher.dispatch ~dispatcher:dispatch ~message
    end
  | _ when key = tab -> begin
      stop_default_behavior ev;

      this##setState (object%js
        val confirmed = not this##.state##.confirmed
      end)
    end
  | _ -> ()

let component =
  let render this =
    let props = this##.props in
    let children = [|
      R.Dom.of_tag `div
        ~props:R.(element_spec ~class_name:"dialog-ConfirmDialog" ())
        ~children:[|
          header ~key:"header" ~title:Js.(to_string this##.props##.title);

          body ~key:"body" ~children:[|
            R.create_element ~key:"content" ~props:(object%js
              val content = props##.content
            end) Content.component;

            button_container ~key:"button_container" ~children:[|
              R.create_element ~key:"yes" ~props:(object%js
                val text = Js.string "Yes"
                val selected = Js.bool this##.state##.confirmed
              end) Button.component;

              R.create_element ~key:"no" ~props:(object%js
                val text = Js.string "No"
                val selected = Js.bool @@ not this##.state##.confirmed
              end) Button.component;
            |]
          |]
        |]
    |] in

    R.create_element C_dialog_base.component ~props:(object%js
      val _open = Js.bool true
      val horizontalCenter = Js.bool true
      val verticalCenter = Js.bool true
      val keyHandler = Js.Optdef.return (key_handler ~dispatch:props##.dispatch ~this);
    end) ~children in
  Component.make
    R.(component_spec
         ~constructor:(fun this props ->
             this##.state := object%js
               val confirmed = true
             end
           )
         ~should_component_update:(fun this _ _ -> true)
         render)
