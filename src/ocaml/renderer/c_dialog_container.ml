module C = Sxfiler_common
module R = Jsoo_reactjs
module M = C.Message

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Dispatcher.t Js.readonly_prop
    end
  end)

let make_dialog ~props = function
  | C.Types.Dialog_type.Confirmation data -> begin
      let module T = C.Types in
      R.create_element ~key:"dialog" ~props:(object%js
        val state = props##.state
        val dispatch = props##.dispatch
        val title = Js.string data.title
        val content = Js.string data.content
        val onComplete = data.on_complete
      end) C_confirmation_dialog.component
    end
  | C.Types.Dialog_type.Name_input data -> begin
      let module T = C.Types in
      R.create_element ~key:"dialog" ~props:(object%js
        val title = Js.string data.title
        val state = props##.state
        val dispatch = props##.dispatch
        val onExecute = data.on_execute
      end) C_name_input_dialog.component
    end
  | C.Types.Dialog_type.Jump -> R.create_element ~key:"dialog" ~props:(object%js
                             val state = props##.state
                             val dispatch = props##.dispatch
                           end) C_jump_dialog.component
  | C.Types.Dialog_type.History -> R.create_element ~key:"dialog" ~props:(object%js
                                     val state = props##.state
                                     val dispatch = props##.dispatch
                                   end) C_history_dialog.component
  | C.Types.Dialog_type.Change_permission -> R.create_element ~key:"dialog" ~props:(object%js
                                               val state = props##.state
                                               val dispatch = props##.dispatch
                                             end) C_permission_dialog.component

let component = Component.make (fun props ->
    let state = props##.state in
    match C.State.(state.dialog_state) with
    | C.State.Dialog_state.Close -> R.empty ()
    | C.State.Dialog_state.Open typ -> make_dialog ~props:props typ
  )
