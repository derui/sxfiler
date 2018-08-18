module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

module type Get = sig
  type params = unit
  type result = T.Configuration.t

  val handle: params -> result Lwt.t
end

(** The gateway for Use Case of {!Rpc.Configuration.Get} *)
module Get(Usecase:Usecase.Configuration.Get) = struct
  type params = unit

  type result = T.Configuration.t

  let handle () =
    match%lwt Usecase.execute () with
    | Ok result -> Lwt.return @@ Translator.Configuration.of_domain result
    | Error _ -> assert false   (* Can not route this branch. *)
end

module type Store = sig
  type params = T.Configuration.t
  type result = unit

  val handle: params -> result Lwt.t
end

(** The gateway for use case of {!Rpc.Configuration.Store} *)
module Store(Usecase:Usecase.Configuration.Store) : Store = struct
  type params = T.Configuration.t

  type result = unit

  let handle params =
    match%lwt Usecase.execute @@ Translator.Configuration.to_domain params with
    | Ok () -> Lwt.return_unit
    | Error _ -> assert false
end
