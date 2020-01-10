open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module G = Sxfiler_server_generated

module List_all = struct
  module Type = struct
    type input = G.Bookmark.ListAllRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.Bookmark.ListAllResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.List_all.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok result ->
          let bookmarks = List.map T.Bookmark.of_domain result in
          Lwt.return_ok G.Bookmark.ListAllResponse.{ bookmarks }
      | Error () -> Gateway_error.(Unknown_error "unknown error") |> Lwt.return_error
  end
end

module Register = struct
  module Type = struct
    type input = G.Bookmark.RegisterRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.Bookmark.RegisterResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.Register.S) : S = struct
    include Type

    let handle (param : G.Bookmark.RegisterRequest.t) =
      let path = param.path in
      match%lwt Usecase.execute { path = Path.of_string path } with
      | Ok result ->
          let res =
            G.Bookmark.RegisterResponse.{ bookmark = T.Bookmark.of_domain result |> Option.some }
          in
          Lwt.return_ok res
      | Error `Conflict -> Gateway_error.(Bookmark_conflict) |> Lwt.return_error
  end
end

module Delete = struct
  module Type = struct
    type input = G.Bookmark.DeleteRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = G.Bookmark.DeleteResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.Delete.S) : S = struct
    include Type

    let handle (param : G.Bookmark.DeleteRequest.t) =
      let id = param.id in
      let id' =
        match Uuidm.of_string id with
        | None -> Error Gateway_error.(Unknown_error "invalid identity format")
        | Some id -> Ok id
      in
      match id' with
      | Error e -> Lwt.return_error e
      | Ok id -> (
          match%lwt Usecase.execute { id } with
          | Ok result ->
              let res =
                G.Bookmark.DeleteResponse.
                  { deletedBookmark = T.Bookmark.of_domain result |> Option.some }
              in
              Lwt.return_ok res
          | Error `Not_found -> Gateway_error.(Bookmark_not_found) |> Lwt.return_error )
  end
end
