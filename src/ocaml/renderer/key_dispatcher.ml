module K = Sxfiler_common.Key_map
module E = Reactjscaml.Event

type t = (Sxfiler_common.Event.IPC.t -> unit) Js.callback

let make ipc =
  Js.wrap_callback @@ (fun channel ->
      let module E = Sxfiler_common.Event in
      E.IPC.send ~channel ~ipc)

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  {K.key = Js.to_string v##.key;
   shift = Js.to_bool v##.shiftKey;
   meta = Js.to_bool v##.altKey;
   ctrl = Js.to_bool v##.ctrlKey;
  }

let dispatch : t -> state:Sxfiler_common.State.t ->
  ev:Reactjscaml.Event.Keyboard_event.t ->
  key_map:K.t -> unit = fun dispatcher ~state ~ev ~key_map ->
  let module E = Sxfiler_common.Event in
  let module KE = Reactjscaml.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown -> ()
  | _ as k -> begin
      let key = keyboard_event_to_key ev in

      let open Sxfiler_common.Util.Option.Infix in
      K.dispatch ~key_map ~key >>= (fun action ->
          let message = Action_mapper.to_message state action in
          Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject message|]
        )
      |> ignore

    end
