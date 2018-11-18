module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module S = Sxfiler_renderer_store

let to_message callback timeouts notification =
  [%c
    P_notification_item.t ~key:notification.T.Notification.id
      ~props:
        (object%js
          val item = notification

          val onItemTimeouted = callback

          val timeouted = List.mem notification.T.Notification.id timeouts
        end)]

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method messages : T.Notification.t list Js.readonly_prop

            method timeouts : string list Js.readonly_prop

            method onItemTimeouted : (string -> unit) Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let children =
          List.map (to_message props##.onItemTimeouted props##.timeouts) props##.messages
        in
        [%e ul ~class_name:"fp-NotificationList" children] )
