open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module D = Sxfiler_domain

(** the gateway for {!module:Usecase.Filer.Make}*)
module Make = struct
  (** request and response definition *)
  module Type = struct
    type params =
      { initial_location : string [@key "initialLocation"]
      ; name : string }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = T.Filer.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  (** Return implementation with some dependency modules *)
  module Make (System : System.S) (U : Usecase.Filer.Make.S) : S = struct
    include Type

    let handle param =
      let params =
        { U.initial_location = Path.of_string param.initial_location |> Path.resolve (module System)
        ; name = param.name }
      in
      match%lwt U.execute params with
      | Ok t -> T.Filer.of_domain t |> Lwt.return
      | Error `Already_exists -> Lwt.fail Gateway_error.(Gateway_error filer_already_exists)
  end
end

(** the gateway for {!module:Usecase.Filer.Get}*)
module Get = struct
  (** request and response for gateway *)
  module Type = struct
    type params = {name : string} [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Filer.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Get.S) : S = struct
    include Type

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> T.Filer.of_domain s |> Lwt.return
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
  end
end

(** the gateway for {!module:Usecase.Filer.Move_parent} *)
module Move_parent = struct
  (** gateway for Move_parent use case. *)
  module Type = struct
    type params = {name : string} [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Filer.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Move_parent.S) : S = struct
    include Type

    let handle param =
      let params = {U.name = param.name} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return @@ T.Filer.of_domain s
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
  end
end

(** The gateway for {!module:Usecase.Filer.Enter_directory} use case. *)
module Enter_directory = struct
  (** Request and response of gateway *)
  module Type = struct
    type params =
      { name : string
      ; item_id : string [@key "itemId"] }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = T.Filer.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  (** Return implementation with dependency modules *)
  module Make (U : Usecase.Filer.Enter_directory.S) : S = struct
    include Type

    let handle param =
      let params = {U.name = param.name; item_id = param.item_id} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return @@ T.Filer.of_domain s
      | Error `Not_found_filer -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
      | Error `Not_found_item -> Lwt.fail Gateway_error.(Gateway_error Item_not_found)
      | Error `Not_directory -> Lwt.fail Gateway_error.(Gateway_error filer_not_directory)
  end
end

module Toggle_mark = struct
  module Type = struct
    type params =
      { name : string
      ; item_ids : string list [@key "itemIds"] }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = T.Filer.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  module Make (U : Usecase.Filer.Toggle_mark.S) : S = struct
    include Type

    let handle params =
      let params = {U.name = params.name; item_ids = params.item_ids} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return @@ T.Filer.of_domain s
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
  end
end

module Move = struct
  module Type = struct
    type params =
      { source : string
      ; dest : string
      ; item_ids : string list [@key "itemIds"] }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result =
      { task_id : string [@key "taskId"]
      ; task_name : string [@key "taskName"] }
    [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  module Make (U : Usecase.Filer.Move.S) : S = struct
    include Type

    let handle params =
      let params = {U.source = params.source; dest = params.dest; item_ids = params.item_ids} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return {task_id = s.task_id |> Uuidm.to_string; task_name = s.task_name}
      | Error (`Not_found _) -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
      | Error `Same_filer -> Lwt.fail Gateway_error.(Gateway_error filer_same_filer)
  end
end

module Delete = struct
  module Type = struct
    type params =
      { source : string
      ; item_ids : string list [@key "itemIds"] }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result =
      { task_id : string [@key "taskId"]
      ; task_name : string [@key "taskName"] }
    [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  module Make (U : Usecase.Filer.Delete.S) : S = struct
    include Type

    let handle params =
      let params = {U.source = params.source; item_ids = params.item_ids} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return {task_id = s |> Uuidm.to_string; task_name = "Delete"}
      | Error (`Not_found _) -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
  end
end

module Copy = struct
  module Type = struct
    type params =
      { source : string
      ; dest : string
      ; item_ids : string list [@key "itemIds"] }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result =
      { task_id : string [@key "taskId"]
      ; task_name : string [@key "taskName"] }
    [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = sig
    include module type of Type
    include Core.Gateway with type params := params and type result := result
  end

  module Make (U : Usecase.Filer.Copy.S) : S = struct
    include Type

    let handle params =
      let params = {U.source = params.source; dest = params.dest; item_ids = params.item_ids} in
      match%lwt U.execute params with
      | Ok s -> Lwt.return {task_id = s |> Uuidm.to_string; task_name = "Copy"}
      | Error (`Not_found _) -> Lwt.fail Gateway_error.(Gateway_error filer_not_found)
  end
end
