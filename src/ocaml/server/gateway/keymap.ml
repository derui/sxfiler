module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module type Get = sig
  type params = unit [@@deriving of_yojson]
  type result = T.Key_map.t [@@deriving to_yojson]

  val handle : params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get (Usecase : Usecase.Keymap.Get.S) : Get = struct
  type params = unit [@@deriving of_yojson]
  type result = T.Key_map.t [@@deriving to_yojson]

  let handle () =
    match%lwt Usecase.execute () with
    | Ok result -> Lwt.return @@ T.Key_map.of_domain result
    | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
end

module type Store = sig
  type params = T.Key_map.t [@@deriving of_yojson]
  type result = unit [@@deriving to_yojson]

  val handle : params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Store (Usecase : Usecase.Keymap.Store.S) : Store = struct
  type params = T.Key_map.t [@@deriving of_yojson]
  type result = unit [@@deriving to_yojson]

  let handle param =
    match%lwt Usecase.execute @@ T.Key_map.to_domain param with
    | Ok () -> Lwt.return_unit
    | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
end

module Add_context = struct
  module type S = sig
    type params = {context : string} [@@deriving of_yojson]
    type result = T.Key_map.t [@@deriving to_yojson]

    val handle : params -> result Lwt.t
  end

  (** The gateway for Use Case of {!Usecase.Keymap.Enable_context} *)
  module Make (Usecase : Usecase.Keymap.Add_context.S) : S = struct
    type params = {context : string} [@@deriving of_yojson]
    type result = T.Key_map.t [@@deriving to_yojson]

    let handle param =
      match%lwt Usecase.execute {context = param.context} with
      | Ok keymap -> Lwt.return @@ T.Key_map.of_domain keymap
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end

module Delete_context = struct
  module type S = sig
    type params = {context : string} [@@deriving of_yojson]
    type result = T.Key_map.t [@@deriving to_yojson]

    val handle : params -> result Lwt.t
  end

  (** The gateway for Use Case of {!Usecase.Keymap.Disable_context} *)
  module Make (Usecase : Usecase.Keymap.Delete_context.S) : S = struct
    type params = {context : string} [@@deriving of_yojson]
    type result = T.Key_map.t [@@deriving to_yojson]

    let handle param =
      match%lwt Usecase.execute {context = param.context} with
      | Ok keymap -> Lwt.return @@ T.Key_map.of_domain keymap
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end
