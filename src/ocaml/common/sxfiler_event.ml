module Mes = Sxfiler_message
module FFI = Sxfiler_ffi
module T = Sxfiler_types
module S = Sxfiler_state
module K = Sxfiler_kbd

(* Event name to request file informations in the directory *)
module IPC = struct

  module Core = struct
    type event_type = Reactjscaml.Event.Keyboard_event.event_type
    type t =
      | Update of S.js Js.t
      | Action of Mes.t
    [@@deriving variants]
  end

  module Listener = struct
    type 'a listener = FFI.Event.t Js.t -> 'a -> unit

    type t =
      | Update of Core.t listener
      | Action of Core.t listener
    [@@deriving variants]
  end

  include Core

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
