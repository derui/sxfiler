module K = Sxfiler_common.Key_map
module E = Jsoo_reactjs.Event

type t = Sxfiler_common.Message.t -> unit

let make ipc =
  fun channel ->
    let module E = Sxfiler_common.Event in
    E.IPC.send ~channel:(E.IPC.action channel) ~ipc

let dispatch ~dispatcher ~message = dispatcher message

let dispatch_key: t -> state:Sxfiler_common.State.t ->
  ev:Jsoo_reactjs.Event.Keyboard_event.t ->
  key_map:K.t -> bool = fun dispatcher ~state ~ev ~key_map ->
  let module E = Sxfiler_common.Event in
  let module KE = Jsoo_reactjs.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = Util.keyboard_event_to_key ev in

      let open Minimal_monadic_caml.Option.Infix in
      let result = K.dispatch ~key_map ~key >|= (fun action ->
          let message = Action_mapper.to_message state action in
          dispatcher message
        ) >|= (fun () -> true)in
      Sxfiler_common.Util.Option.get ~default:false result
    end
