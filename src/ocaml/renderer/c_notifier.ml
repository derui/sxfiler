module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module S = Sxfiler_renderer_store

let to_message notification =
  [%c
    P_notifier_item.t
      ~props:
        (object%js
          val item = notification
        end)]

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
        let children = List.map to_message state.notifications in
        [%e div ~class_name:"fp-NotifierWrapper" [%e ul ~class_name:"fp-Notifier" children]] )
