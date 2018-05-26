module C = Sxfiler_common
module M = C.Message

type message = M.t
type key_action = C.Callable_action.t
type action = Context.t -> C.State.t Lwt.t

(** action implementation to move cursor to next/prev *)
let move_focus ~amount ctx =
  let open Context in
  let waiter, waker = Lwt.wait () in
  let open Lwt.Infix in
  Rpc.request
    ctx.rpc
    (module Api.Pane.Move_focus)
    (Lwt.wakeup waker) (Some amount)
  >>= fun () -> waiter
  >>= fun server_state -> Lwt.return {ctx.state with C.State.server_state}

(** action implementation to enter into focusing directory *)
let enter_directory ctx =
  let open Context in
  let waiter, waker = Lwt.wait () in
  let open Lwt.Infix in
  Rpc.request
    ctx.rpc
    (module Api.Pane.Enter_directory)
    (Lwt.wakeup waker) None
  >>= fun () -> waiter
  >>= fun server_state -> Lwt.return {ctx.state with C.State.server_state}

(** action implementation to leave from current directory *)
let leave_directory ctx =
  let open Context in
  let waiter, waker = Lwt.wait () in
  let open Lwt.Infix in
  Rpc.request
    ctx.rpc
    (module Api.Pane.Up_directory)
    (Lwt.wakeup waker) None
  >>= fun () -> waiter
  >>= fun server_state -> Lwt.return {ctx.state with C.State.server_state}

let open_confirmation_for_copy ctx =
  let module D = C.State.Dialog_state in
  let module S = C.State in
  let module SS = C.Server_state in
  let active = ctx.Context.state.S.server_state.SS.active_pane
  and inactive = ctx.Context.state.S.server_state.SS.inactive_pane in
  let active_dir = active.C.Types.Pane.directory
  and inactive_dir = inactive.C.Types.Pane.directory in
  let dialog_state = D.Open (C.Types.Dialog_type.Confirmation {
      title = "Copy file";
      on_complete = (fun () -> M.Copy_files);
      content = Printf.sprintf "Are you ready to copy files from %s to %s ?" active_dir inactive_dir;
    })
  in
  Lwt.return {ctx.Context.state with dialog_state}

(** Send quit event to main process *)
let quit_application ctx =
  let electron = Modules.electron in
  let ipc = electron##.ipcRenderer in
  ipc##send Js.(string "quit") () |> ignore;
  Lwt.return ctx.Context.state

(** Create action that is executable with renderer context *)
let create = function
  | C.Callable_action.Core action -> begin
      let module Core = C.Callable_action.Core in
      match action with
      | Core.Jump -> fun ctx -> Lwt.return @@ {ctx.Context.state with
                                               dialog_state = let module D = C.State.Dialog_state in
                                                 D.Open C.Types.Dialog_type.Jump
                                              }
      | Next_item -> fun ctx -> move_focus ~amount:1 ctx
      | Prev_item -> fun ctx -> move_focus ~amount:(-1) ctx
      | Enter_directory -> fun ctx -> enter_directory ctx
      | Leave_directory -> fun ctx -> leave_directory ctx
      | Copy -> fun ctx -> open_confirmation_for_copy ctx
      | Quit -> fun ctx -> quit_application ctx
      | Unknown action -> fun ctx -> Lwt.fail_with @@ Printf.sprintf "Unknown action for core module: %s" action
      | _ -> failwith "not implemented yet"
    end
  | _ -> fun _ -> Lwt.fail_with "Not implemented yet for modules excluded Core module."

(* Functions to create action from Message *)
let copy ctx =
  let open Context in
  let waiter, waker = Lwt.wait () in
  let open Lwt.Infix in
  Rpc.request
    ctx.rpc
    (module Api.File.Copy)
    (Lwt.wakeup waker) None
  >>= fun () -> waiter
  >>= fun server_state -> Lwt.return {ctx.state with C.State.server_state}

let rec create_from_message = function
  | M.Close_dialog -> fun ctx -> Lwt.return {ctx.Context.state with
                                             dialog_state = C.State.Dialog_state.Close}
  | M.Close_dialog_with_action action -> begin
      fun ctx ->
        let action = create_from_message action in
        let open Lwt.Infix in
        (Lwt.return {ctx.Context.state with dialog_state = C.State.Dialog_state.Close})
        >>= (fun state -> Lwt.return {ctx with Context.state})
        >>= action
    end
  | M.Copy_files -> fun ctx -> copy ctx
  | _ -> failwith "not implemented yet"
