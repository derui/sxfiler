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
module Get (Usecase : Usecase.Keymap.Get.S) = struct
  type params = unit
  type result = T.Key_map.t

  let handle () =
    match%lwt Usecase.execute () with
    | Ok result -> Lwt.return @@ Translator.Key_map.of_domain result
    | Error _ -> assert false
end

module type Store = sig
  type params = T.Key_map.t
  type result = unit

  val handle : params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Store (Usecase : Usecase.Keymap.Store.S) = struct
  type params = T.Key_map.t
  type result = unit

  let handle param =
    match%lwt Usecase.execute @@ Translator.Key_map.to_domain param with
    | Ok () -> Lwt.return_unit
    | Error _ -> assert false
end

module Add_context = struct
  module type S = sig
    type params = {context : string} [@@deriving yojson]
    type result = T.Key_map.t

    val handle : params -> result Lwt.t
  end

  (** The gateway for Use Case of {!Usecase.Keymap.Enable_context} *)
  module Make (Usecase : Usecase.Keymap.Add_context.S) : S = struct
    type params = {context : string} [@@deriving yojson]
    type result = T.Key_map.t

    let handle param =
      match%lwt Usecase.execute {context = param.context} with
      | Ok keymap -> Lwt.return @@ Translator.Key_map.of_domain keymap
      | Error _ -> assert false

    (* Can not route this branch. *)
  end
end

module Delete_context = struct
  module type S = sig
    type params = {context : string} [@@deriving yojson]
    type result = T.Key_map.t

    val handle : params -> result Lwt.t
  end

  (** The gateway for Use Case of {!Usecase.Keymap.Disable_context} *)
  module Make (Usecase : Usecase.Keymap.Delete_context.S) : S = struct
    type params = {context : string} [@@deriving yojson]
    type result = T.Key_map.t

    let handle param =
      match%lwt Usecase.execute {context = param.context} with
      | Ok keymap -> Lwt.return @@ Translator.Key_map.of_domain keymap
      | Error _ -> assert false

    (* Can not route this branch. *)
  end
end
