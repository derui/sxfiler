open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module G = Sxfiler_server_generated.Filer
module D = Sxfiler_domain

(** the gateway for {!module:Usecase.Filer.Make}*)
module Make = struct
  (** request and response definition *)
  module Type = struct
    type input = G.FilerMakeRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerMakeResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** Return implementation with some dependency modules *)
  module Make (System : System.S) (U : Usecase.Filer.Make.S) : S = struct
    include Type

    let handle (param : G.FilerMakeRequest.t) =
      let input =
        {
          U.initial_location = Path.of_string param.initialLocation |> Path.resolve (module System);
          name = param.name;
        }
      in
      match%lwt U.execute input with
      | Ok t -> { G.FilerMakeResponse.filer = T.Filer.of_domain t |> Option.some } |> Lwt.return_ok
      | Error `Already_exists -> Lwt.return_error Gateway_error.(Filer_already_exists)
  end
end

(** the gateway for {!module:Usecase.Filer.Get}*)
module Get = struct
  (** request and response for gateway *)
  module Type = struct
    type input = G.FilerGetRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerGetResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    let input_from_pb = G.FilerGetRequest.from_proto
    let output_to_pb = G.FilerGetResponse.to_proto
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Get.S) : S = struct
    include Type

    let handle (param : G.FilerGetRequest.t) =
      let input = { U.name = param.name } in
      match%lwt U.execute input with
      | Ok s -> { G.FilerGetResponse.filer = T.Filer.of_domain s |> Option.some } |> Lwt.return_ok
      | Error `Not_found -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end

(** the gateway for {!module:Usecase.Filer.Move_parent} *)
module Move_parent = struct
  (** gateway for Move_parent use case. *)
  module Type = struct
    type input = G.FilerMoveParentRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerMoveParentResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** Return implementation with some dependency modules *)
  module Make (U : Usecase.Filer.Move_parent.S) : S = struct
    include Type

    let handle (param : G.FilerMoveParentRequest.t) =
      let input = { U.name = param.name } in
      match%lwt U.execute input with
      | Ok s ->
          { G.FilerMoveParentResponse.filer = T.Filer.of_domain s |> Option.some } |> Lwt.return_ok
      | Error `Not_found -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end

(** The gateway for {!module:Usecase.Filer.Enter_directory} use case. *)
module Enter_directory = struct
  (** Request and response of gateway *)
  module Type = struct
    type input = G.FilerEnterDirectoryRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerEnterDirectoryResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** Return implementation with dependency modules *)
  module Make (U : Usecase.Filer.Enter_directory.S) : S = struct
    include Type

    let handle (param : input) =
      let input = { U.name = param.name; item_id = param.itemId } in
      match%lwt U.execute input with
      | Ok s ->
          { G.FilerEnterDirectoryResponse.filer = T.Filer.of_domain s |> Option.some }
          |> Lwt.return_ok
      | Error `Not_found_filer -> Lwt.return_error Gateway_error.(Filer_not_found)
      | Error `Not_found_item -> Lwt.return_error Gateway_error.(Item_not_found)
      | Error `Not_directory -> Lwt.return_error Gateway_error.(Filer_not_directory)
  end
end

module Toggle_mark = struct
  module Type = struct
    type input = G.FilerToggleMarkRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerToggleMarkResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Filer.Toggle_mark.S) : S = struct
    include Type

    let handle (input : input) =
      let input = { U.name = input.name; item_ids = input.itemIds } in
      match%lwt U.execute input with
      | Ok s ->
          { G.FilerToggleMarkResponse.filer = T.Filer.of_domain s |> Option.some } |> Lwt.return_ok
      | Error `Not_found -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end

module Move = struct
  module Type = struct
    type input = G.FilerMoveRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerMoveResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Filer.Move.S) : S = struct
    include Type

    let handle (input : input) =
      let input = { U.source = input.source; dest = input.dest; item_ids = input.itemIds } in
      match%lwt U.execute input with
      | Ok s ->
          Lwt.return_ok
            { G.FilerMoveResponse.taskId = s.task_id |> Uuidm.to_string; taskName = s.task_name }
      | Error (`Not_found _) -> Lwt.return_error Gateway_error.(Filer_not_found)
      | Error `Same_filer -> Lwt.return_error Gateway_error.(Filer_same_filer)
  end
end

module Delete = struct
  module Type = struct
    type input = G.FilerDeleteRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerDeleteResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Filer.Delete.S) : S = struct
    include Type

    let handle (input : input) =
      let input = { U.source = input.source; item_ids = input.itemIds } in
      match%lwt U.execute input with
      | Ok s ->
          Lwt.return_ok { G.FilerDeleteResponse.taskId = s |> Uuidm.to_string; taskName = "Delete" }
      | Error (`Not_found _) -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end

module Copy = struct
  module Type = struct
    type input = G.FilerCopyRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerCopyResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (U : Usecase.Filer.Copy.S) : S = struct
    include Type

    let handle (input : input) =
      let input = { U.source = input.source; dest = input.dest; item_ids = input.itemIds } in
      match%lwt U.execute input with
      | Ok s ->
          Lwt.return_ok { G.FilerCopyResponse.taskId = s |> Uuidm.to_string; taskName = "Copy" }
      | Error (`Not_found _) -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end

(** the gateway for {!module:Usecase.Filer.Jump_location}*)
module Jump_location = struct
  (** request and response definition *)
  module Type = struct
    type input = G.FilerJumpLocationRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.FilerJumpLocationResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** Return implementation with some dependency modules *)
  module Make (System : System.S) (U : Usecase.Filer.Jump_location.S) : S = struct
    include Type

    let handle (param : input) =
      let input =
        {
          U.location = Path.of_string param.location |> Path.resolve (module System);
          name = param.name;
        }
      in
      match%lwt U.execute input with
      | Ok t ->
          { G.FilerJumpLocationResponse.filer = T.Filer.of_domain t |> Option.some }
          |> Lwt.return_ok
      | Error `Not_found -> Lwt.return_error Gateway_error.(Filer_not_found)
  end
end
