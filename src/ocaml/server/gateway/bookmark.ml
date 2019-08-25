open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module List_all = struct
  module Type = struct
    type params = unit [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = T.Bookmark.t list
    [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.List_all.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok result -> List.map T.Bookmark.of_domain result |> Lwt.return
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end

module Register = struct
  module Type = struct
    type params = {path : string} [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Bookmark.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.Register.S) : S = struct
    include Type

    let handle {path} =
      match%lwt Usecase.execute {path = Path.of_string path} with
      | Ok result -> T.Bookmark.of_domain result |> Lwt.return
      | Error `Conflict -> Lwt.fail Gateway_error.(Gateway_error bookmark_conflict)
  end
end

module Delete = struct
  module Type = struct
    type params = {id : string} [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Bookmark.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Bookmark.Delete.S) : S = struct
    include Type

    let handle {id} =
      let%lwt id' =
        match Uuidm.of_string id with
        | None -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "invalid identity format"))
        | Some id -> Lwt.return id
      in
      match%lwt Usecase.execute {id = id'} with
      | Ok result -> T.Bookmark.of_domain result |> Lwt.return
      | Error `Not_found -> Lwt.fail Gateway_error.(Gateway_error bookmark_not_found)
  end
end
