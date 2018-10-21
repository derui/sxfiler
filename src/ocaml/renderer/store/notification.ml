module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module Tr = Sxfiler_renderer_translator

module Notification_set = Set.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

type progress =
  { process : string
  ; target : float
  ; current : float }

module State = struct
  type message = C.Message.t

  type t =
    { notifications : T.Notification.t list
    ; progresses : progress list
    ; timeouts : Notification_set.t }

  let make () = {notifications = []; timeouts = Notification_set.empty; progresses = []}

  let reduce t = function
    | C.Message.Notify_message v -> {t with notifications = v :: t.notifications}
    | C.Message.Notify_progress v -> (
        match v.body with
        | OneShot _ -> t
        | Progress v ->
          let new_progress = {process = v.process; target = v.targeted; current = v.current} in
          if List.exists (fun p -> v.process = p.process) t.progresses then
            { t with
              progresses =
                List.map
                  (fun p -> if p.process = new_progress.process then new_progress else p)
                  t.progresses }
          else {t with progresses = new_progress :: t.progresses} )
    | C.Message.Timeout_notification_message id ->
      {t with timeouts = Notification_set.add id t.timeouts}
    | C.Message.Delete_notification_progress process ->
      let without_target v = v.process <> process in
      {t with progresses = List.filter without_target t.progresses}
    | C.Message.Delete_notification_message id ->
      let without_target v = v.T.Notification.id <> id in
      { t with
        notifications = List.filter without_target t.notifications
      ; timeouts = Notification_set.remove id t.timeouts }
    | _ -> t

  let equal = ( = )
end

module Store = C.Store.Make (State)
