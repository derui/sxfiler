(** This module defines behavior for default key handler. *)
open Sxfiler_core
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core
module R = Jsoo_reactjs

type t = {
  key_map: T.Key_map.t;
}

type constructor = T.Key_map.t
type param = R.Event.Keyboard_event.t
type result = unit

let action_to_message = function
  | T.Callable_action.Core core -> begin
      match core with
      | T.Callable_action.Core.Quit -> Some C.Message.Quit
      | _ -> None
    end
  | _ -> None

let handle_key_event ~ev ~key_map =
  let module K = T.Key_map in
  let module KE = R.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      let open Option.Infix in
      let result = K.find key_map ~key
        >>= fun action -> action_to_message action
        >|= (fun _ -> true) in
      Option.is_some result
    end

let make key_map = {key_map}

let execute t ev =
  let module C = T.Configuration in
  let dispatched = handle_key_event ~ev ~key_map:t.key_map in
  if dispatched then begin
    ev##preventDefault; ev##stopPropagation
  end else ()
