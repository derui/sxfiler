module C = Sxfiler_common
module R = Jsoo_reactjs
module M = C.Message

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
    end
  end)

let make_dialog ~props = function
  | C.Types.Dialog_confirmation task -> begin
      let module T = C.Types in
      match task with
      | `Copy -> R.create_element ~key:"dialog" ~props:(object%js
                   val state = props##.state
                   val dispatch = props##.dispatch
                   val title = Js.string "Confirm copy file"
                   val content = Js.string "Can filer copy file?"
                   val onComplete = fun () -> T.User_action.Confirm (T.Task_request.(to_js Copy))
                 end) C_confirmation_dialog.component
      | `Delete -> R.create_element ~key:"dialog" ~props:(object%js
                     val state = props##.state
                     val dispatch = props##.dispatch
                     val title = Js.string "Confirm delete file"
                     val content = Js.string "Can delete file?"
                     val onComplete = fun () -> T.User_action.Confirm (T.Task_request.(to_js Delete))
                   end) C_confirmation_dialog.component
      | `Move -> R.create_element ~key:"dialog" ~props:(object%js
                   val state = props##.state
                   val dispatch = props##.dispatch
                   val title = Js.string "Confirm move file"
                   val content = Js.string "Can move file?"
                   val onComplete = fun () -> T.User_action.Confirm (T.Task_request.(to_js Move))
                 end) C_confirmation_dialog.component
      | _ -> R.empty ()
    end
  | C.Types.Dialog_name_input task -> begin
      match task with
      | `Rename -> R.create_element ~key:"dialog" ~props:(object%js
                     val title = Js.string "Rename object"
                     val state = props##.state
                     val dispatch = props##.dispatch
                     val onExecute = fun v -> C.Types.Task_request.(to_js @@ Rename v)
                   end) C_name_input_dialog.component
      | `Mkdir -> R.create_element ~key:"dialog" ~props:(object%js
                    val title = Js.string "Make directory"
                    val state = props##.state
                    val dispatch = props##.dispatch
                    val onExecute = fun v -> C.Types.Task_request.(to_js @@ Mkdir v)
                  end) C_name_input_dialog.component
      | _ -> R.empty ()
    end
  | C.Types.Dialog_jump -> R.create_element ~key:"dialog" ~props:(object%js
                             val state = props##.state
                             val dispatch = props##.dispatch
                           end) C_jump_dialog.component
  | C.Types.Dialog_history -> R.create_element ~key:"dialog" ~props:(object%js
                                val state = props##.state
                                val dispatch = props##.dispatch
                              end) C_history_dialog.component

let component = Component.make (fun props ->
    let state = props##.state in
    match C.State.(state.dialog_state) with
    | C.State.Dialog_state.Close -> R.empty ()
    | C.State.Dialog_state.Open typ -> make_dialog ~props:props typ
  )
