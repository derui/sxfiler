module FFI = Sxfiler_ffi
module T = Sxfiler_types

(* Event name to request file informations in the directory *)
module IPC = struct
  module Listener = struct
    type 'a listener = FFI.Event.t Js.t -> 'a -> unit
    type t = [
        `REQUEST_FILES_IN_DIRECTORY of string listener
      | `FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array) listener
      | `REQUEST_QUIT_APPLICATION of unit listener
    ]
    [@@deriving variants]

  end

  type t = [
      `REQUEST_FILES_IN_DIRECTORY of string
    | `FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array)
    | `REQUEST_QUIT_APPLICATION
  ] [@@deriving variants]

  (* Simple wrapper to regist listener to EventEmitter *)
  let on: target:('a Listener.listener -> Listener.t) -> f:('a Listener.listener) -> FFI.ipc Js.t -> unit
    = fun ~target ~f emitter ->
      let typ = target f in
      let channel = Listener.Variants.to_name typ |> Js.string in
      let listener = Js.wrap_callback f in
      emitter##on channel listener

  (* Simple wrapper to regist one-time listener to EventEmitter *)
  let once: target:('a Listener.listener -> Listener.t) -> f:'a Listener.listener -> FFI.ipc Js.t -> unit
    = fun ~target ~f emitter ->
      let typ = target f in
      let channel = Listener.Variants.to_name typ |> Js.string in
      let listener = Js.wrap_callback f in
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
