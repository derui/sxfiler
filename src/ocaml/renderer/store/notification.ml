module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module Tr = Sxfiler_renderer_translator

module State = struct
  type message = C.Message.t
  type t = {notifications : T.Notification.t list}

  let make () = {notifications = []}

  let reduce t = function
    | C.Message.Notify v -> {notifications = v :: t.notifications}
    | C.Message.Timeout_notification id ->
      {notifications = List.filter (fun v -> v.T.Notification.id = id) t.notifications}
    | _ -> t

  let equal = ( = )
end

module Store = C.Store.Make (State)
