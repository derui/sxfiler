open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module Gen = Sxfiler_server_generated

module Get = struct
  module Type = struct
    type input = Gen.Keymap.KeymapGetRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.Keymap.KeymapGetResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Get.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output ->
          let res =
            Gen.Keymap.KeymapGetResponse.{ keymap = Option.some @@ T.Key_map.of_domain output }
          in
          Lwt.return_ok res
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Reload = struct
  module Type = struct
    type input = Gen.Keymap.KeymapReloadRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.Keymap.KeymapReloadResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Keymap.Reload.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output ->
          let res =
            Gen.Keymap.KeymapReloadResponse.{ keymap = Option.some @@ T.Key_map.of_domain output }
          in
          Lwt.return_ok res
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Store = struct
  module Type = struct
    type input = Gen.Keymap.KeymapStoreRequest.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

    type output = Gen.Keymap.KeymapStoreResponse.t
    [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Store.S) : S = struct
    include Type

    let handle (param : Gen.Keymap.KeymapStoreRequest.t) =
      match param.keymap with
      | Some param -> (
          match%lwt Usecase.execute @@ T.Key_map.to_domain param with
          | Ok () -> Lwt.return_ok ()
          | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error") )
      | None -> Lwt.return_error Gateway_error.(Unknown_error "invalid request")
  end
end
