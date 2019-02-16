module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Notify_message = struct
  module type S = sig
    type params =
      { message : string
      ; level : T.Notification.Level.t }
    [@@deriving yojson]

    val handle : params -> unit Lwt.t
  end

  module Make (Usecase : Usecase.Notification.Notify.S) : S = struct
    type params =
      { message : string
      ; level : T.Notification.Level.t }
    [@@deriving yojson]

    let handle param =
      let notification = D.Notification.Message param.message in
      let level = T.Notification.Level.to_domain param.level in
      match%lwt Usecase.execute {notification; level} with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Errors.(Gateway_error (unknown_error "unknown error"))
  end
end
