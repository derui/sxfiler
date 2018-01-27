module E = Reactjscaml.Event

type t = (Sxfiler_common.Event.IPC.t -> unit) Js.callback

let make ipc =
  Js.wrap_callback @@ (fun channel ->
      let module E = Sxfiler_common.Event in
      E.IPC.send ~channel ~ipc)

let dispatch : t -> Reactjscaml.Event.keyboard_event -> unit = fun dispatcher ev ->
  let module E = Sxfiler_common.Event in
  let channel = match Js.to_string ev##._type with
    | "keydown" -> Some (E.IPC.keydown ev)
    | "keyup" -> Some (E.IPC.keyup ev)
    | "keypress" -> Some (E.IPC.keypress ev)
    | _ -> None
  in
  match channel with
  | None -> ()
  | Some channel -> Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject channel|]
