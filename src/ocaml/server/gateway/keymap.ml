module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Get = struct
  module Type = struct
    type params = unit [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Key_map.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Get.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok result -> Lwt.return @@ T.Key_map.of_domain result
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end

module Reload = struct
  module Type = struct
    type params = unit [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = T.Key_map.t [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  module Make (Usecase : Usecase.Keymap.Reload.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok result -> Lwt.return @@ T.Key_map.of_domain result
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end

module Store = struct
  module Type = struct
    type params = T.Key_map.t [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
    type result = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type params = Type.params and type result = Type.result

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Store.S) : S = struct
    include Type

    let handle param =
      match%lwt Usecase.execute @@ T.Key_map.to_domain param with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end
