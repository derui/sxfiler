module C_= Sxfiler_common
module K = Sxfiler_common.Key_map
module E = Jsoo_reactjs.Event

let handle_key_event: Dispatcher.t -> state:Sxfiler_common.State.t ->
  ev:Jsoo_reactjs.Event.Keyboard_event.t ->
  key_map:K.t -> bool = fun dispatcher ~state ~ev ~key_map ->
  let module E = Sxfiler_common.Event in
  let module KE = Jsoo_reactjs.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = Util.keyboard_event_to_key ev in

      let open Minimal_monadic_caml.Option.Infix in
      let result = K.dispatch ~key_map ~key
        >>= (fun action -> Action_mapper.to_message state action)
        >>= (fun message -> Some (dispatcher message))
        >|= (fun () -> true)in
      Sxfiler_common.Util.Option.get ~default:false result
    end
