module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

module type Get = sig
  type params = unit
  type result = T.Key_map.t

  val handle : params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get (Usecase : Usecase.Keymap.Get) = struct
  type params = unit
  type result = T.Key_map.t

  let handle () =
    match%lwt Usecase.execute () with
    | Ok result ->
      Lwt.return @@ Translator.Key_map.of_domain result
    | Error _ ->
      assert false
end

module type Store = sig
  type params = T.Key_map.t
  type result = unit

  val handle : params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Store (Usecase : Usecase.Keymap.Store) = struct
  type params = T.Key_map.t
  type result = unit

  let handle param =
    match%lwt Usecase.execute @@ Translator.Key_map.to_domain param with
    | Ok () ->
      Lwt.return_unit
    | Error _ ->
      assert false
end

module type Enable_context = sig
  type params = {context : string} [@@deriving yojson]
  type result = T.Key_map.t

  val handle : params -> result Lwt.t
end

(** The gateway for Use Case of {!Usecase.Keymap.Enable_context} *)
module Enable_context (Usecase : Usecase.Keymap.Enable_context) : Enable_context = struct
  type params = {context : string} [@@deriving yojson]
  type result = T.Key_map.t

  let handle param =
    match%lwt Usecase.execute {context = param.context} with
    | Ok keymap ->
      Lwt.return @@ Translator.Key_map.of_domain keymap
    | Error _ ->
      assert false

  (* Can not route this branch. *)
end

module type Disable_context = sig
  type params = {context : string} [@@deriving yojson]
  type result = T.Key_map.t

  val handle : params -> result Lwt.t
end

(** The gateway for Use Case of {!Usecase.Keymap.Disable_context} *)
module Disable_context (Usecase : Usecase.Keymap.Disable_context) : Disable_context = struct
  type params = {context : string} [@@deriving yojson]
  type result = T.Key_map.t

  let handle param =
    match%lwt Usecase.execute {context = param.context} with
    | Ok keymap ->
      Lwt.return @@ Translator.Key_map.of_domain keymap
    | Error _ ->
      assert false

  (* Can not route this branch. *)
end
