module C = Sxfiler_common
module R = Jsoo_reactjs

module Component = R.Component.Make_stateful
    (struct
      class type t = object
        method dispatch : Dispatcher.t Js.readonly_prop
        method state : C.State.t Js.readonly_prop
      end
    end)
    (struct
      type t = unit
    end)

let handle_execute ~dispatch ~this selected =
  let module M = C.Message in
  match selected with
  | None -> None
  | Some selected -> Some (M.Jump_directory selected)

(* Get completion list element *)
let item_renderer item selected =
  let class_name = "dialog-HistoryDialogCompleter_Item" in
  R.Dom.of_tag `div ~props:(R.element_spec ~class_name ()) ~children:[
    R.Dom.of_tag `span ~props:(R.element_spec
                                 ~key:"description"
                                 ~class_name:"dialog-HistoryDialogCompleter_ItemLabel" ())
      ~children:[R.text item]
  ]

module History_completion_list = C_completion_list.Make(struct
    type t = string

    let to_id v = v
  end)

(* Get completion list element *)
let completion_list_renderer ~this key selected =
  let state = this##.props##.state in
  let items = C.(state.State.completer_state.Completer_state.matched_items) in
  R.create_element ~key ~props:(object%js
    val items = items
    val selectedIndex = selected
    val itemRenderer = item_renderer
  end) History_completion_list.component

let render this =
  let props = this##.props in

  R.create_element C_file_completion_dialog.component ~props:(object%js
    val dispatch = props##.dispatch
    val state = props##.state
    val title = Js.string "Back to history"
    val onExecute = handle_execute ~dispatch:props##.dispatch ~this
    val completionListRenderer = completion_list_renderer ~this
  end)

let component = Component.make @@ R.component_spec render
