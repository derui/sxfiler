module C = Sxfiler_common
module R = Jsoo_reactjs

type current_rane =
    Owner
  | Group
  | Others

module Permission_view = struct
  module Component = R.Component.Make_stateless(struct
      class type t = object
        method currentRane: current_rane Js.readonly_prop
        method owner: int Js.readonly_prop
        method group: int Js.readonly_prop
        method others: int Js.readonly_prop
      end
    end)

  let perm_cell text allowed =
    R.Dom.of_tag `td ~key:text
      ~children:[|
        R.Dom.of_tag `span
          ~props:(R.element_spec ~class_name:"dialog-PermissionDialog_PermissionCell" ())
          ~children:[|
            R.Dom.of_tag `span
              ~key:"name"
              ~props:(R.element_spec ~class_name:"dialog-PermissionDialog_PermissionName" ())
              ~children:[|R.text text|];
            R.Dom.of_tag `input ~key:"checkbox"
              ~props:(R.element_spec () ~others:(object%js
                        val _type = "checkbox"
                        val checked = allowed
                      end))
          |]
      |]

  let rane_to_row perm current =
    let read = (perm land 0o4) <> 0
    and write = (perm land 0o2) <> 0
    and exec = (perm land 0o1) <> 0 in
    let class_name =
      let open Classnames in
      return "dialog-PermissionDialog_PermissionRow"
      <|> ("dialog-PermissionDialog_PermissionRow-selected", current)
      |> to_string
    in

    R.Dom.of_tag `tr ~props:(R.element_spec ~class_name ()) ~children:[|
      perm_cell "Read" read;
      perm_cell "Write" write;
      perm_cell "eXec" exec;
    |]

  let component = Component.make (fun props ->
      R.Dom.of_tag `table ~props:(R.element_spec ~class_name:"dialog-PermissionDialog_PermissionViewer" ())
        ~children:[|
          R.Dom.of_tag `tbody ~children:[|
            rane_to_row props##.owner (props##.currentRane = Owner);
            rane_to_row props##.group (props##.currentRane = Group);
            rane_to_row props##.others (props##.currentRane = Others);
          |]
        |]
    )
end

let text_container ~key ~children =
  let class_name = Classnames.(return "dialog-PermissionDialog_TextContainer" |> to_string) in
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name ()) ~children

let header ~key ~title =
  R.Dom.of_tag `div ~key
    ~props:R.(element_spec ~class_name:"dialog-PermissionDialog_Header" ())
    ~children:[|R.text title|]

let body ~key ~children =
  R.Dom.of_tag `div ~key ~props:R.(element_spec ~class_name:"dialog-PermissionDialog_Body" ()) ~children

module Component = R.Component.Make_stateful
    (struct
      class type t = object
        method dispatch : Key_dispatcher.t Js.readonly_prop
        method state : C.State.t Js.readonly_prop
      end
    end)
    (struct
      class type t = object
        method currentRane: current_rane Js.readonly_prop
        method owner: int Js.readonly_prop
        method group: int Js.readonly_prop
        method others: int Js.readonly_prop
      end
    end)

let make_state ?currentRane ?owner ?group ?others current =
  object%js
    val currentRane = C.Util.Option.get ~default:current##.currentRane currentRane
    val owner = C.Util.Option.get ~default:current##.owner owner
    val group = C.Util.Option.get ~default:current##.group group
    val others = C.Util.Option.get ~default:current##.others others
  end

let handle_cancel ~dispatch =
  let module M = C.Message in
  let message = M.close_dialog @@ C.Types.User_action.(to_js Cancel) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message

let make_permission state =
  let owner = state##.owner
  and group = state##.group
  and others = state##.others in
  (owner * 0o100) + (group * 0o10) + others

let split_permission mode =
  let owner = (mode land 0o700) / 0o100
  and group = (mode land 0o70) / 0o10
  and others = mode land 0o7 in
  (owner, group, others)

let handle_submit ~dispatch v =
  let module M = C.Message in
  let module T = C.Types in
  let task = T.Task_request.(to_js @@ Change_permission (make_permission v)) in
  let action =  T.(User_action.to_js @@ User_action.Confirm task) in
  Key_dispatcher.dispatch ~dispatcher:dispatch ~message:(M.close_dialog action)

let esc = Sxfiler_kbd.(to_keyseq {empty with key = "Escape"})
let enter_key = Sxfiler_kbd.(to_keyseq {empty with key = "Enter"})
let key_for_read = Sxfiler_kbd.(to_keyseq {empty with key = "r"})
let key_for_write = Sxfiler_kbd.(to_keyseq {empty with key = "w"})
let key_for_exec = Sxfiler_kbd.(to_keyseq {empty with key = "x"})
let arrow_up = Sxfiler_kbd.(to_keyseq {empty with key = "ArrowUp"})
let arrow_down = Sxfiler_kbd.(to_keyseq {empty with key = "ArrowDown"})

let next_rane = function
  | Owner -> Group
  | Group -> Others
  | Others -> Others

let prev_rane = function
  | Owner -> Owner
  | Group -> Owner
  | Others -> Group

let update_rane this = function
  | `Up -> this##setState (make_state ~currentRane:(prev_rane this##.state##.currentRane) this##.state)
  | `Down -> this##setState (make_state ~currentRane:(next_rane this##.state##.currentRane) this##.state)

let toggle_permission this perm =
  let current_rane = this##.state##.currentRane in
  let flag = match perm with
    | `Read -> 0o4
    | `Write -> 0o2
    | `Exec -> 0o1
  in
  match current_rane with
  | Owner -> this##setState (make_state ~owner:(this##.state##.owner lxor flag) this##.state)
  | Group -> this##setState (make_state ~group:(this##.state##.group lxor flag) this##.state)
  | Others -> this##setState (make_state ~others:(this##.state##.others lxor flag) this##.state)

let key_handler ~this ev =

  let key = Util.keyboard_event_to_key ev |> Js.to_string in
  match key with
  | _ when key = enter_key -> handle_submit ~dispatch:this##.props##.dispatch this##.state
  | _ when key = esc -> handle_cancel ~dispatch:this##.props##.dispatch
  | _ when key = arrow_up -> update_rane this `Up
  | _ when key = arrow_down -> update_rane this `Down
  | _ when key = key_for_read -> toggle_permission this `Read
  | _ when key = key_for_write -> toggle_permission this `Write
  | _ when key = key_for_exec -> toggle_permission this `Exec
  | _ -> ()

let component =
  let render this =
    let props = this##.props in
    let module T = C.Types.File_stat in
    let pane = C.State.(active_pane props##.state) in
    let focused_item =
      let open Minimal_monadic_caml.Option.Infix in
      let module P = C.Types.Pane in
      pane.P.focused_item >>= fun (id, _) -> P.find_item ~id pane
    in
    match focused_item with
    | None -> R.empty ()
    | Some focused_item -> begin

        R.create_element C_dialog_base.component ~props:(object%js
          val _open = Js.bool true
          val horizontalCenter = Js.bool true
          val verticalCenter = Js.bool false
          val keyHandler = Js.Optdef.return @@ key_handler ~this
        end) ~children:[|
          R.Dom.of_tag `div
            ~props:R.(element_spec ~class_name:"dialog-PermissionDialog" ())
            ~children:[|
              header ~key:"header" ~title:"Edit permission of file";
              body ~key:"body" ~children:[|
                R.create_element ~key:"input" ~props:(object%js
                  val currentRane = this##.state##.currentRane
                  val owner = this##.state##.owner
                  val group = this##.state##.group
                  val others = this##.state##.others
                end) Permission_view.component;

                text_container ~key:"text_container" ~children:[|
                  R.text "Enter: execute; Esc: cancel"
                |]
              |]
            |]
        |]
      end
  in

  Component.make R.(
      component_spec
        ~constructor:(fun this props ->
            let module T = C.Types.File_stat in
            let module Pane = C.Types.Pane in
            let pane = C.State.(active_pane props##.state) in
            let open Minimal_monadic_caml.Option in
            let open Infix in
            this##.state := object%js
              val currentRane = Owner
              val owner = 0
              val group = 0
              val others = 0
            end;

            pane.Pane.focused_item
            >>= (fun (id, _) -> Pane.find_item ~id pane)
            >>= lift (fun item ->
                let owner, group, others = Js.float_of_number item.T.stat##.mode
                                           |> int_of_float
                                           |> split_permission
                in
                this##.state := make_state ~owner ~group ~others this##.state
              )
            |> ignore
          )
        render
    )
