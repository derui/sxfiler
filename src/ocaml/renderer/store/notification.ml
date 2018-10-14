module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module Tr = Sxfiler_renderer_translator

module Notification_set = Set.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

module State = struct
  type message = C.Message.t

  type t =
    { notifications : T.Notification.t list
    ; timeouts : Notification_set.t }

  let make () = {notifications = []; timeouts = Notification_set.empty}

  let reduce t = function
    | C.Message.Notify v -> {t with notifications = v :: t.notifications}
    | C.Message.Timeout_notification id -> {t with timeouts = Notification_set.add id t.timeouts}
    | C.Message.Delete_notification id ->
      let without_target v = v.T.Notification.id <> id in
      { notifications = List.filter without_target t.notifications
      ; timeouts = Notification_set.remove id t.timeouts }
    | _ -> t

  let equal = ( = )
end

module Store = C.Store.Make (State)
