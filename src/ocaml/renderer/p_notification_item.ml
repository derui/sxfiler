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
            method item : T.Notification.t Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let item = props##.item in
        let class_name =
          Classnames.to_string
            [ ("fp-NotificationList_Item", true)
            ; ("fp-NotificationList_Item-info", item.T.Notification.level = D.Notification.Level.Info)
            ; ( "fp-NotificationList_Item-warning"
              , item.T.Notification.level = D.Notification.Level.Warning )
            ; ( "fp-NotificationList_Item-error"
              , item.T.Notification.level = D.Notification.Level.Error ) ]
        in
        match item.body with
        | D.Notification.OneShot v -> [%e li ~class_name [(v.message) [@txt]]]
        | D.Notification.Progress _ -> R.empty () )
