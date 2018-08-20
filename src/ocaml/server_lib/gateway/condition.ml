module Usecase = Sxfiler_usecase
module Translator = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

module type Enable = sig
  type params = {
    context: string;
  } [@@deriving yojson]

  type result = unit

  val handle: params -> result Lwt.t
end

(** The gateway for Use Case of {!Usecase.Condition.Enable} *)
module Enable(Usecase:Usecase.Condition.Enable) = struct
  type params = {
    context: string
  } [@@deriving yojson]

  type result = unit

  let handle param =
    match%lwt Usecase.execute {context = param.context} with
    | Ok () -> Lwt.return_unit
    | Error _ -> assert false   (* Can not route this branch. *)
end

module type Disable = sig
  type params = {
    context: string;
  } [@@deriving yojson]

  type result = unit

  val handle: params -> result Lwt.t
end

(** The gateway for Use Case of {!Usecase.Condition.Disable} *)
module Disable(Usecase:Usecase.Condition.Disable) = struct
  type params = {
    context: string
  } [@@deriving yojson]

  type result = unit

  let handle param =
    match%lwt Usecase.execute {context = param.context} with
    | Ok () -> Lwt.return_unit
    | Error _ -> assert false   (* Can not route this branch. *)
end
