module E = Reactjscaml.Event

type t = (Sxfiler_common.Event.IPC.t -> unit) Js.callback

let make ipc =
  Js.wrap_callback @@ (fun channel ->
      let module E = Sxfiler_common.Event in
      E.IPC.send ~channel ~ipc)

let dispatch : t -> Reactjscaml.Event.Keyboard_event.t -> unit = fun dispatcher ev ->
  let module E = Sxfiler_common.Event in
  let module K = Reactjscaml.Event.Keyboard_event in
  let channel = match K.to_event_type ev with
    | K.Unknown -> None
    | _ -> Some (E.IPC.request_key_handling ev)
  in
  match channel with
  | None -> ()
  | Some channel -> Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject channel|]
