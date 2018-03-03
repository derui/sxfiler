module C = Sxfiler_common
module R = Jsoo_reactjs

module Input = struct
  module C = R.Component.Make_stateful(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
        method onRef: (Dom_html.element Js.t -> unit) Js.readonly_prop
        method onCancel: (unit -> unit) Js.readonly_prop
        method onSubmit: (string -> unit) Js.readonly_prop
      end
    end)(struct
      class type t = object
        method value: Js.js_string Js.t Js.readonly_prop
      end
    end)

  let input_handler ~this ev =
    match Dom_html.tagged ev##.target with
    | Dom_html.Input e ->
      let value = e##.value in
      this##setState (object%js
        val value = value
      end)
    | _ -> ()

  let esc = Sxfiler_kbd.(to_keyseq {empty with key = "Escape"})
  let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
  let key_handler ~this ev =

    let key = Util.keyboard_event_to_key ev |> Js.to_string in
    match key with
    | _ when key = enter_key -> this##.props##.onSubmit @@ Js.to_string this##.state##.value
    | _ when key = esc -> this##.props##.onCancel ()
    | _ -> ()

  let component =
    let render this =
      let props = this##.props in
      R.Dom.of_tag `div
        ~props:R.(element_spec ~class_name:"dialog-RenameDialog_InputContainer" ())
        ~children:[|
          R.Dom.of_tag `span ~key:"label"
            ~props:R.(element_spec ~class_name:"dialog_RenameDialog_InputLabel" ())
            ~children:[| R.text "Name:" |];
          R.Dom.of_tag `input ~key:"input"
            ~_ref:props##.onRef
            ~props:R.(element_spec ()
                        ~class_name:"dialog-RenameDialog_Input"
                        ~on_key_down:(key_handler ~this)
                        ~on_change:(input_handler ~this)
                        ~default_value:(Js.to_string this##.state##.value)
                        ~others:(object%js
                          val tabIndex = "1"
                        end))
        |]
    in
    C.make R.(component_spec
                ~constructor:(fun this props ->
                    this##.state := object%js
                      val value = props##.text
                    end)
                render)
end

let text_container ~key ~children =
  let class_name = Classnames.(return "dialog-RenameDialog_TextContainer" |> to_string) in
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name ()) ~children


module Component = R.Component.Make_stateful
    (struct
      class type t = object
        method dispatch : Key_dispatcher.t Js.readonly_prop
        method state : C.State.t Js.readonly_prop
      end
    end)
    (struct
      type t = unit
    end)

let handle_cancel ~dispatch () =
  let module M = C.Message in
  let message = M.finish_user_action @@ C.Types.User_action.(to_js Cancel) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message

let handle_submit ~dispatch v =
  let module M = C.Message in
  let task = C.Types.Task.(to_js @@ Rename v) in
  let action =  C.Types.(User_action.to_js @@ User_action.Confirm task) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.finish_user_action action)

let component = Component.make
    R.(component_spec
         ~constructor:(fun this props -> this##.nodes := Jstable.create ();)
         ~component_did_mount:(fun this ->
             match R.Ref_table.find ~key:"input" this##.nodes with
             | Some e -> e##focus
             | None -> ()
           )
         ~component_did_update:(fun this _ _ ->
             match R.Ref_table.find ~key:"input" this##.nodes with
             | Some e -> e##focus
             | None -> ()
           )
         ~component_will_unmount:(fun this ->
             R.Ref_table.remove ~key:"input" this##.nodes
           )
         (fun this ->
            let props = this##.props in
            let module T = C.Types.File_stat in
            let selected_file = C.State.(active_pane props##.state |> Pane.pointed_file) in
            let original = selected_file.T.filename in

            R.create_element C_dialog_base.component ~props:(object%js
              val title = Js.string "Rename object"
              val _open = Js.bool true
              val keyHandler = Js.Optdef.empty
            end) ~children:[|
              R.Dom.of_tag `div
                ~props:R.(element_spec ~class_name:"dialog-RenameDialog" ())
                ~children:[|
                  R.create_element ~key:"input" ~props:(object%js
                    val text = Js.string original
                    val onRef = (fun e -> R.Ref_table.add ~key:"input" ~value:e this##.nodes)
                    val onCancel = handle_cancel ~dispatch:props##.dispatch
                    val onSubmit = handle_submit ~dispatch:props##.dispatch
                  end) Input.component;

                  text_container ~key:"text_container" ~children:[|
                    R.text "Enter: execute; Esc: cancel"
                  |]

                |]
            |]
         )
      )
