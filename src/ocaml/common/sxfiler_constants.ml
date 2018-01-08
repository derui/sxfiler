module FFI = Sxfiler_ffi
module T = Sxfiler_types

(* Event name to request file informations in the directory *)
module IPC_events = struct
  type t = [
      `REQUEST_FILES_IN_DIRECTORY of string
    | `FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array)
    | `REQUEST_QUIT_APPLICATION
  ] [@@deriving variants]

  (* Simple wrapper to regist listener to EventEmitter *)
  let on: channel:t -> listener:(FFI.Event.t Js.t -> 'a -> unit) -> FFI.ipc Js.t -> unit
    = fun ~channel ~listener emitter ->
      let channel = Variants.to_name channel |> Js.string in
      let listener = Js.wrap_callback listener in
      emitter##on channel listener

  (* Simple wrapper to regist one-time listener to EventEmitter *)
  let once: channel:t -> listener:(FFI.Event.t Js.t -> 'a -> unit) -> FFI.ipc Js.t -> unit
    = fun ~channel ~listener emitter ->
      let channel = Variants.to_name channel |> Js.string in
      let listener = Js.wrap_callback listener in
      emitter##once channel listener

  (* Simple wrapper to reply message from event object *)
  let reply: channel:t -> ev:FFI.Event.t Js.t -> unit
    = fun ~channel ~ev ->
      let v = channel in
      let channel = Variants.to_name channel |> Js.string in
      let sender = ev##.sender in
      sender##(send channel (Js.Opt.return v))

  (* Simple wrapper to send message from event object *)
  let send: channel:t -> ipc:FFI.ipc Js.t -> unit
    = fun ~channel ~ipc ->
      let v = channel in
      let channel = Variants.to_name channel |> Js.string in
      ipc##send channel (Js.Opt.return v)
end
