module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Get = struct
  module Type = struct
    type input = unit [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = T.Key_map.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Get.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output -> Lwt.return_ok @@ T.Key_map.of_domain output
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Reload = struct
  module Type = struct
    type input = unit [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = T.Key_map.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Keymap.Reload.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output -> Lwt.return_ok @@ T.Key_map.of_domain output
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

module Store = struct
  module Type = struct
    type input = T.Key_map.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = unit [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  (** This module defines rpc interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (Usecase : Usecase.Keymap.Store.S) : S = struct
    include Type

    let handle param =
      match%lwt Usecase.execute @@ T.Key_map.to_domain param with
      | Ok () -> Lwt.return_ok ()
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end
