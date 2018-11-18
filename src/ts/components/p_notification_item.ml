module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module TG = Jsoo_reactjs_transition_group
module S = Sxfiler_renderer_store

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method item : T.Notification.t Js.readonly_prop

            method timeouted : bool Js.readonly_prop

            method onItemTimeouted : (string -> unit) Js.readonly_prop
          end
      end )
    ~spec:
      R.(
        component_spec
          ~initial_state:(fun _ _ ->
              object%js
                val mounted = false
              end )
          ~initial_custom:(fun _ _ -> object%js end)
          ~component_did_mount:(fun this ->
              this##setState
                (object%js
                  val mounted = true
                end) )
          (fun this ->
             let item = this##.props##.item in
             TG.css_transition
               ~_in:(this##.state##.mounted && not this##.props##.timeouted)
               ~on_exited:(fun _ -> this##.props##.onItemTimeouted item.T.Notification.id)
               ~timeout:200 ~class_name:"fp-NotificationList_ItemAnimation"
               (fun _ ->
                  let class_name =
                    Classnames.to_string
                      [ ("fp-NotificationList_Item", true)
                      ; ( "fp-NotificationList_Item-info"
                        , item.T.Notification.level = D.Notification.Level.Info )
                      ; ( "fp-NotificationList_Item-warning"
                        , item.T.Notification.level = D.Notification.Level.Warning )
                      ; ( "fp-NotificationList_Item-error"
                        , item.T.Notification.level = D.Notification.Level.Error ) ]
                  in
                  match item.body with
                  | D.Notification.OneShot v -> [%e li ~class_name [(v.message [@txt])]]
                  | D.Notification.Progress _ -> R.empty () ) ))
