module C = Sxfiler_common
module R = Jsoo_reactjs

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

let handle_execute ~dispatch ~this () =
  let module M = C.Message in
  let state = this##.props##.state in
  let current_selected = C.State.(File_completion_state.selected state.file_completion_state) in
  let directory = current_selected.C.Types.File_stat.directory in
  M.jump_location @@ Js.string directory

(* Get completion list element *)
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

module File_completion_list = C_completion_list.Make(struct
    type t = C.Types.File_stat.t

    let to_id v = C.Types.(File_id.to_string v.File_stat.id)
  end)

(* Get completion list element *)
let completion_list_renderer ~this key =
  let state = this##.props##.state in
  let completion_state = C.State.(state.file_completion_state) in
  let items = completion_state.C.State.File_completion_state.items
  and selected = completion_state.C.State.File_completion_state.cursor_pos in
  R.create_element ~key ~props:(object%js
    val items = items
    val selectedIndex = selected
    val itemRenderer = item_renderer
  end) File_completion_list.component

let render this =
  let props = this##.props in

  R.create_element C_file_completion_dialog.component ~props:(object%js
    val dispatch = props##.dispatch
    val state = props##.state
    val title = Js.string "Jump to location"
    val onExecute = handle_execute ~dispatch:props##.dispatch ~this
    val completionListRenderer = completion_list_renderer ~this
  end)

let component = Component.make @@ R.component_spec render
