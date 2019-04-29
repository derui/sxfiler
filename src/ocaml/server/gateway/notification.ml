module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator

module Notify_message = struct
  module Type = struct
    type params =
      { message : string
      ; level : T.Notification.Level.t }
    [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]

    type result = unit [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module Make (Usecase : Usecase.Notification.Notify.S) :
    Core.Gateway with type params = Type.params and type result = Type.result = struct
    include Type

    let handle param =
      let notification = D.Notification.Message param.message in
      let level = T.Notification.Level.to_domain param.level in
      match%lwt Usecase.execute {notification; level} with
      | Ok () -> Lwt.return_unit
      | Error () -> Lwt.fail Gateway_error.(Gateway_error (unknown_error "unknown error"))
  end
end
