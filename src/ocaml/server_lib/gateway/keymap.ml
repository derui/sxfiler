module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator

module type Get = sig
  type params = unit
  type result = Translator.Key_map.t

  val handle: params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get(Usecase:Usecase.Keymap.Get) = struct
  type params = unit
  type result = Translator.Key_map.t

  let handle () =
    match%lwt Usecase.execute () with
    | Ok result -> Lwt.return @@ Translator.Key_map.of_domain result
    | Error _ -> assert false
end

module type Store = sig
  type params = Translator.Key_map.t
  type result = unit

  val handle: params -> result Lwt.t
end

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Store(Usecase:Usecase.Keymap.Store) = struct
  type params = Translator.Key_map.t
  type result = unit

  let handle param =
    match%lwt Usecase.execute @@ Translator.Key_map.to_domain param with
    | Ok () -> Lwt.return_unit
    | Error _ -> assert false
end
