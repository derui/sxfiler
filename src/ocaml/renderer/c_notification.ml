module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module S = Sxfiler_renderer_store

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method locator : (module Locator.S) Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let module L = (val props##.locator) in
        let state =
          S.App.Store.get L.store |> S.App.State.notification |> S.Notification.Store.get
        in
        [%e
          div ~class_name:"fp-NotificationContainer"
            [ [%c
              P_notification_list.t
                ~props:
                  (object%js
                    val messages = state.notifications
                  end)] ]] )
