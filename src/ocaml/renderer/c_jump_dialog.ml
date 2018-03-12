module C = Sxfiler_common
module R = Jsoo_reactjs

module Input = struct
  module C = R.Component.Make_stateful(struct
      class type t = object
        method onRef: (Dom_html.element Js.t -> unit) Js.readonly_prop
        method onInput: (Js.js_string Js.t -> unit) Js.readonly_prop
        method onCancel: (unit -> unit) Js.readonly_prop
        method onSubmit: (unit -> unit) Js.readonly_prop
        method onMoveCursor: ([ `Up | `Down] -> unit) Js.readonly_prop
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
      end);
      this##.props##.onInput value
    | _ -> ()

  let esc = Sxfiler_kbd.(to_keyseq {empty with key = "Escape"})
  let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
  let arrow_up = Sxfiler_kbd.(to_keyseq {empty with key = "ArrowUp"})
  let arrow_down = Sxfiler_kbd.(to_keyseq {empty with key = "ArrowDown"})
  let key_handler ~this ev =

    let key = Util.keyboard_event_to_key ev |> Js.to_string in
    match key with
    | _ when key = enter_key -> this##.props##.onSubmit ()
    | _ when key = esc -> this##.props##.onCancel ()
    | _ when key = arrow_up -> this##.props##.onMoveCursor `Up
    | _ when key = arrow_down -> this##.props##.onMoveCursor `Down
    | _ -> ()

  let component =
    let render this =
      let props = this##.props in
      R.Dom.of_tag `div
        ~props:R.(element_spec ~class_name:"dialog-JumpDialog_InputContainer" ())
        ~children:[|
          R.Dom.of_tag `span ~key:"label"
            ~props:R.(element_spec ~class_name:"dialog-JumpDialog_InputLabel" ())
            ~children:[| R.text "Name:" |];
          R.Dom.of_tag `input ~key:"input"
            ~_ref:props##.onRef
            ~props:R.(element_spec ()
                        ~class_name:"dialog-JumpDialog_Input"
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
                      val value = Js.string ""
                    end)
                render)
end

let text_container ~key ~children =
  let class_name = Classnames.(return "dialog-JumpDialog_TextContainer" |> to_string) in
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
  let message = M.close_dialog @@ C.Types.User_action.(to_js Cancel) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message

let handle_submit ~dispatch ~this v =
  let module M = C.Message in
  let state = this##.props##.state in
  let current_selected = C.State.(File_completion.selected state.file_completion_state) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.jump_location @@ C.Types.File_stat.to_js current_selected)

let handle_input ~dispatch path =
  let module M = C.Message in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.refresh_candidates_request path)

let handle_cursor ~dispatch = function
  | `Up ->
    let module M = C.Message in
    Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.select_prev_completion)
  | `Down ->
    let module M = C.Message in
    Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.select_next_completion)

(* Get completion list element *)
let completion_list ~key ~completion_state =
  let items = completion_state.C.State.File_completion.items
  and selected = completion_state.C.State.File_completion.cursor_pos in
  Firebug.console##log items;
  let item_renderer item selected =
    let class_name = "dialog-JumpDialogCompleter_Item" in
    R.Dom.of_tag `div ~props:(R.element_spec ~class_name ()) ~children:[|
      R.Dom.of_tag `span ~props:(R.element_spec
                                   ~key:"description"
                                   ~class_name:"dialog-JumpDialogCompleter_ItemDirectory" ())
        ~children:[|R.text item.C.Types.File_stat.directory|];
      R.Dom.of_tag `span ~props:(R.element_spec
                                   ~key:"text"
                                   ~class_name:"dialog-JumpDialogCompleter_ItemName" ())
        ~children:[|R.text item.C.Types.File_stat.filename|];
    |]
  in
  R.create_element ~key ~props:(object%js
    val items = items
    val selectedIndex = selected
    val itemRenderer = item_renderer
  end) C_file_completion_list.component

let component =
  let render this =
    let props = this##.props in
    let state = props##.state in

    R.create_element C_dialog_base.component ~props:(object%js
      val _open = Js.bool true
      val horizontalCenter = Js.bool true
      val verticalCenter = Js.bool false
      val keyHandler = Js.Optdef.empty
    end) ~children:[|
      R.Dom.of_tag `div
        ~props:R.(element_spec ~class_name:"dialog-JumpDialog" ())
        ~children:[|
          R.create_element ~key:"input" ~props:(object%js
            val onRef = (fun e -> R.Ref_table.add ~key:"input" ~value:e this##.nodes)
            val onInput = handle_input ~dispatch:props##.dispatch
            val onCancel = handle_cancel ~dispatch:props##.dispatch
            val onSubmit = handle_submit ~dispatch:props##.dispatch ~this
            val onMoveCursor = handle_cursor ~dispatch:props##.dispatch
          end) Input.component;

          text_container ~key:"text_container" ~children:[|
            R.text "Enter: execute; Esc: cancel"
          |];

          completion_list ~key:"completion_list" ~completion_state:C.State.(state.file_completion_state)
        |]
    |]
  in
  Component.make @@ R.component_spec
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
    render
