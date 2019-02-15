module D = Sxfiler_domain
module Usecase = Sxfiler_usecase

module Notify_message = struct
  module type S = sig
    type params =
      { message : string
      ; level : int }
    [@@deriving yojson]

    type result = {invalid_level : bool}

    val handle : params -> result Lwt.t
  end

  module Make (Usecase : Usecase.Notification.Notify.S) : S = struct
    type params =
      { message : string
      ; level : int }
    [@@deriving yojson]

    type result = {invalid_level : bool}

    let handle param =
      let notification = D.Notification.Message param.message in
      let level = D.Notification.Level.of_int param.level in
      match level with
      | None -> Lwt.return {invalid_level = true}
      | Some level -> (
          match%lwt Usecase.execute {notification; level} with
          | Ok () -> Lwt.return {invalid_level = false}
          | Error _ -> assert false )
  end
end
