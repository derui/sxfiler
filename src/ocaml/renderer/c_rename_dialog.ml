module C = Sxfiler_common
module R = Jsoo_reactjs

module Input = struct
  module C = R.Component.Make_stateful(struct
      class type t = object
        method text: Js.js_string Js.t Js.readonly_prop
        method onRef: (Dom_html.element Js.t -> unit) Js.readonly_prop
      end
    end)(struct
      class type t = object
        method value: Js.js_string Js.t Js.readonly_prop
      end
    end)

  let input_handler ~this ev =
    ev##stopPropagation;
    let value = ev##.target##getAttribute (Js.string "value") in
    this##setState (object%js
      val value = Js.Opt.get value (fun () -> Js.string "")
    end)

  let component = C.make R.(component_spec
      ~constructor:(fun this props ->
          this##.state := object%js
            val value = props##.text
          end)
      (fun this ->
          let props = this##.props in
          R.Dom.of_tag `div
            ~props:R.(element_spec ~class_name:"dialog-RenameDialog_InputContainer" ())
            ~children:[|
              R.Dom.of_tag `span
                ~props:R.(element_spec ~class_name:"dialog_RenameDialog_InputLabel" ())
                ~children:[| R.text "Name:" |];
              R.Dom.of_tag `input
                ~_ref:props##.onRef
                ~props:R.(element_spec ()
                    ~class_name:"dialog-RenameDialog_Input"
                    ~on_change:(input_handler ~this)
                    ~default_value:(Js.to_string this##.state##.value)
                    ~others:(object%js
                      val tabIndex = "1"
                    end))
            |]
        )
    )
end

let text_container ~key ~children =
  let class_name = Classnames.(return "dialog-RenameDialog_TextContainer" |> to_string) in
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name ()) ~children

let esc = Sxfiler_kbd.(to_keyseq {empty with key = "Escape"})
let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
let key_handler ~dispatch ~this ev =
  ev##preventDefault;
  ev##stopPropagation;

  let key = Util.keyboard_event_to_key ev |> Js.to_string in
  let module M = C.Message in
  match key with
  | _ when key = enter_key ->
    let task = C.Types.Task.(to_js @@ Rename Js.(to_string this##.state##.value)) in
    let action =  C.Types.(User_action.to_js @@ User_action.Confirm task) in
    Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.finish_user_action action)
  | _ when key = esc ->
    let message = M.finish_user_action @@ C.Types.User_action.(to_js Cancel) in
    Key_dispatcher.dispatch ~dispatcher:dispatch ~message
  | _ -> ()

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

let component = Component.make
    R.(component_spec
    ~constructor:(fun this props -> this##.nodes := Jstable.create ();)
    ~should_component_update:(fun this new_props state -> this##.state##.filename <> state##.filename)
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
              end) Input.component;

              text_container ~key:"text_container" ~children:[|
                R.text "Enter: execute; Esc: cancel"
              |]

            |]
        |]
      )
      )
