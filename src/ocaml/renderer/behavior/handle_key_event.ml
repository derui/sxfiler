(** This module defines behavior for default key handler. *)
open Sxfiler_core
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core
module R = Jsoo_reactjs

type message = C.Message.t
type t = unit

type param = (C.Key_map.t * C.Types.Condition.t * R.Event.Keyboard_event.t)
type result = unit

let action_to_message = function
  | C.Callable_action.Core core -> begin
      match core with
      | _ -> None
    end
  | File_list action -> begin
      match action with
      | C.Callable_action.File_list.Next_item -> Some C.Message.Move_cursor_to_next
      | Prev_item -> Some C.Message.Move_cursor_to_prev
      | _ -> None
    end
  | _ -> None

let event_to_message ~ev ~bindings =
  let module K = C.Key_map in
  let module KE = R.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> None
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      let open Option.Infix in
      C.Key_bindings.find bindings ~key
      >>= fun action -> action_to_message action
    end

let make _ = ()

let execute () dispatcher (key_map, condition, ev) =
  let bindings = C.Key_map.find key_map ~condition in
  let message = event_to_message ~ev ~bindings in
  match message with
  | None -> ()
  | Some message ->
    let module D = (val dispatcher : C.Dispatcher_intf.Instance with type message = C.Message.t) in
    D.Dispatcher.dispatch D.instance message;
    ev##preventDefault; ev##stopPropagation
