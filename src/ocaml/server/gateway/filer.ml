open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module D = Sxfiler_domain

(** the gateway for {!module: Usecase.Filer.Make}*)
module Make = struct
  (** request and response definition *)
  module Types = struct
    type params =
      { initial_location : string [@key "initialLocation"]
      ; name : string }
    [@@deriving of_yojson]
  end

  (** The signature of gateway *)
  module type S = sig
    include module type of Types

    val handle : params -> T.Filer.t Lwt.t
  end

  (** Return implementation with some dependency modules *)
  module Make (System : System.S) (U : Usecase.Filer.Make) : S = struct
    include Types

    let handle param =
      let params =
        { U.initial_location = Path.of_string param.initial_location |> Path.resolve (module System)
        ; name = param.name }
      in
      match%lwt U.execute params with
      | Ok t -> T.Filer.of_domain t |> Lwt.return
      | Error `Already_exists -> Lwt.fail Errors.(Gateway_error filer_already_exists)
  end
end

(** the gateway for {!module: Usecase.Filer.Get}*)
module Get = struct
  (** request and response for gateway *)
  module Types = struct
    type params = {name : string} [@@deriving of_yojson]
  end

  (** The signature of gateway *)
  module type S = sig
    include module type of Types

    val handle : params -> T.Filer.t Lwt.t
  end

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Get) : S = struct
    include Types

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> T.Filer.of_domain s |> Lwt.return
      | Error `Not_found -> Lwt.fail Errors.(Gateway_error filer_not_found)
  end
end

(** the gateway for {!module: Usecase.Filer.Move_parent} *)
module Move_parent = struct
  (** gateway for Move_parent use case. *)
  module Types = struct
    type params = {name : string} [@@deriving of_yojson]
  end

  (** The signature of gateway  *)
  module type S = sig
    include module type of Types

    val handle : params -> T.Filer.t Lwt.t
  end

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Move_parent.S) : S = struct
    include Types

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return @@ T.Filer.of_domain s
      | Error `Not_found -> Lwt.fail Errors.(Gateway_error filer_not_found)
  end
end

(** The gateway for {!module: Usecase.Filer.Enter_directory} use case. *)
module Enter_directory = struct
  (** Request and response of gateway *)
  module Types = struct
    type params =
      { name : string
      ; node_id : string [@key "nodeId"] }
    [@@deriving of_yojson]
  end

  (** The signature of gateway *)
  module type S = sig
    include module type of Types

    val handle : params -> T.Filer.t Lwt.t
  end

  (** Return implementation with dependency modules *)
  module Make (U : Usecase.Filer.Enter_directory.S) : S = struct
    include Types

    let handle param =
      let params = {U.name = param.name; node_id = param.node_id} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return @@ T.Filer.of_domain s
      | Error `Not_found_filer -> Lwt.fail Errors.(Gateway_error filer_not_found)
      | Error `Not_found_node -> Lwt.fail Errors.(Gateway_error filer_not_found_node)
      | Error `Not_directory -> Lwt.fail Errors.(Gateway_error filer_not_directory)
  end
end