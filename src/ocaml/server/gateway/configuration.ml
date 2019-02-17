module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

(** The gateway for Use Case of {!Rpc.Configuration.Get} *)
module Get = struct
  module type S = sig
    type params = unit [@@deriving of_yojson]
    type result = T.Configuration.t [@@deriving to_yojson]

    val handle : params -> result Lwt.t
  end

  module Make (Usecase : Usecase.Configuration.Get.S) : S = struct
    type params = unit [@@deriving of_yojson]
    type result = T.Configuration.t [@@deriving to_yojson]

    let handle () =
      match%lwt Usecase.execute () with
      | Ok result -> Lwt.return @@ T.Configuration.of_domain result
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end

(** The gateway for use case of {!Rpc.Configuration.Store} *)
module Store = struct
  module type S = sig
    type params = T.Configuration.t [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]

    val handle : params -> result Lwt.t
  end

  module Make (Usecase : Usecase.Configuration.Store.S) : S = struct
    type params = T.Configuration.t [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]

    let handle params =
      match%lwt Usecase.execute @@ T.Configuration.to_domain params with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end
