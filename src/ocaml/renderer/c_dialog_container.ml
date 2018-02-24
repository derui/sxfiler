module C = Sxfiler_common
module R = Reactjscaml
module M = C.Message

module Component = R.Component.Make_stateless (struct
    class type t = object
      method state: C.State.t Js.readonly_prop
      method dispatch: Key_dispatcher.t Js.readonly_prop
    end
  end)

let make_dialog ~props = function
  | M.Operation.Copy _ -> R.element ~key:"dialog" ~props:(object%js
                            val state = props##.state
                            val dispatch = props##.dispatch
                            val title = Js.string "Confirm copy file"
                            val content = Js.string "Can filer copy file?"
                          end) C_confirmation_dialog.component
  | M.Operation.Delete _ -> R.element ~key:"dialog" ~props:(object%js
                              val state = props##.state
                              val dispatch = props##.dispatch
                              val title = Js.string "Confirm delete file"
                              val content = Js.string "Can delete file?"
                            end) C_confirmation_dialog.component

let component = Component.make (fun props ->
    let state = props##.state in
    match C.State.(state.operation.Operation.next) with
    | None -> R.empty ()
    | Some typ -> make_dialog ~props:props typ
  )
