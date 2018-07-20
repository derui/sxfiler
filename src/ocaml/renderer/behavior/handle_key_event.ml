(** This module defines behavior for default key handler. *)
open Sxfiler_core
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core
module R = Jsoo_reactjs

type message = C.Message.t
type t = unit

type param = (C.Key_map.t * C.Types.Mode.t * R.Event.Keyboard_event.t)
type result = unit

let action_to_message = function
  | C.Callable_action.Core core -> begin
      match core with
      | C.Callable_action.Core.Quit -> Some C.Message.Quit
      | _ -> None
    end
  | _ -> None

let event_to_message ~ev ~bindings =
  let module K = C.Key_map in
  let module KE = R.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      let open Option.Infix in
      let result = C.Key_bindings.find bindings ~key
        >>= fun action -> action_to_message action
        >|= (fun _ -> true) in
      Option.is_some result
    end

let make _ = ()

let execute () _ (key_map, mode, ev) =
  let bindings = C.Key_map.find key_map ~mode in
  match bindings with
  | None -> ()
  | Some bindings ->
    let dispatched = event_to_message ~ev ~bindings in
    if dispatched then begin
      ev##preventDefault; ev##stopPropagation
    end else ()
