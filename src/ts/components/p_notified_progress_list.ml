(** This component displays progress bars of each processes *)

module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module S = Sxfiler_renderer_store

let to_message callback notification =
  [%c
    P_notified_progress_item.t ~key:notification.S.Notification.process
      ~props:
        (object%js
          val item = notification

          val onProcessFinished = callback
        end)]

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method progresses : S.Notification.progress list Js.readonly_prop

            method onProcessFinished : (string -> unit) Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let children = List.map (to_message props##.onProcessFinished) props##.progresses in
        [%e ul ~class_name:"fp-NotifiedProgressList" children] )
