module D = Sxfiler_domain
module Usecase = Sxfiler_usecase
module T = Sxfiler_rpc.Types

module Notify_message = struct
  module type S = sig
    type params =
      { message : string
      ; level : T.Notification.level }

    type result = unit

    val handle : params -> result Lwt.t
  end

  module Make (Usecase : Usecase.Notification.Notify.S) : S = struct
    type params =
      { message : string
      ; level : T.Notification.level }

    type result = unit

    let handle param =
      let notification = D.Notification.OneShot {message = param.message} in
      match%lwt Usecase.execute {notification; level = param.level} with
      | Ok () -> Lwt.return_unit
      | Error _ -> assert false
  end
end
