open Sxfiler_core
module K = Sxfiler_types.Key_map
module E = Jsoo_reactjs.Event

let handle_key_event: Dispatcher.t ->
  ev:Jsoo_reactjs.Event.Keyboard_event.t ->
  key_map:K.t -> bool = fun dispatcher ~ev ~key_map ->
  let module KE = Jsoo_reactjs.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> false
  | _ -> begin
      let key = Util.keyboard_event_to_key ev in

      let open Option.Infix in
      let result = K.find key_map ~key
        >>= (fun action -> Some (Dispatcher.dispatch ~dispatcher @@ Action_creator.create action))
        >|= (fun () -> true)in
      Option.get ~default:false result
    end
