module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module S = Sxfiler_renderer_store
module C = Sxfiler_renderer_core
module U = Sxfiler_renderer_usecase

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
        let on_item_timeouted id =
          let instance = C.Usecase.make_instance (module U.Delete_notification) ~param:id in
          let module Ctx = (val L.context) in
          Ctx.(Context.execute this instance) |> Lwt.ignore_result
        in
        let on_process_finished process =
          let instance = C.Usecase.make_instance (module U.Delete_progress) ~param:process in
          let module Ctx = (val L.context) in
          Ctx.(Context.execute this instance) |> Lwt.ignore_result
        in
        let timeouts = S.Notification.Notification_set.fold List.cons state.timeouts [] in
        [%e
          div ~class_name:"fp-NotificationContainer"
            [ [%c
              P_notification_list.t ~key:"messages"
                ~props:
                  (object%js
                    val messages = state.notifications

                    val timeouts = timeouts

                    val onItemTimeouted = on_item_timeouted
                  end)]
            ; [%c
              P_notified_progress_list.t ~key:"progresses"
                ~props:
                  (object%js
                    val progresses = state.progresses

                    val onProcessFinished = on_process_finished
                  end)] ]] )
