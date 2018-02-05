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

let dispatch : t -> Reactjscaml.Event.Keyboard_event.t -> unit = fun dispatcher ev ->
  let module E = Sxfiler_common.Event in
  let module K = Reactjscaml.Event.Keyboard_event in
  match K.to_event_type ev with
  | K.Unknown -> ()
  | _ as k -> begin
      let channel = keyboard_event_to_key ev
                    |> Sxfiler_kbd.to_js
                    |> fun v -> E.IPC.request_key_handling (v, k) in
      Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject channel|]
    end
