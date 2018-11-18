(** This component displays progress of a process *)

module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module TG = Jsoo_reactjs_transition_group
module S = Sxfiler_renderer_store

let progress_to_ratio (progress : S.Notification.progress) =
  if progress.target = 0. then 1.0 else min (progress.current /. progress.target) 1.0

let progress_bar ratio =
  let style =
    object%js
      val width = Printf.sprintf "%f%%" ratio
    end
  in
  [%e
    div ~class_name:"fp-NotifiedProgressItem_ProgressBar" ~key:"progress-bar"
      [ [%e
        div ~class_name:"fp-NotifiedProgressItem_ProgressIndicator" ~key:"indicator"
          ~others:
            (object%js
              val style = style
            end)] ]]

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method item : S.Notification.progress Js.readonly_prop

            method onProcessFinished : (string -> unit) Js.readonly_prop
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
             let ratio = progress_to_ratio item *. 100.0 in
             let progress_finished = item.current = item.target in
             TG.css_transition
               ~_in:(this##.state##.mounted && not progress_finished)
               ~on_exited:(fun _ -> this##.props##.onProcessFinished item.S.Notification.process)
               ~timeout:200 ~class_name:"fp-NotifiedProgressItem_Animation"
               (fun _ ->
                  let class_name = "fp-NotifiedProgressItem" in
                  [%e
                    li ~class_name
                      [ [%e
                        span ~key:"process" ~class_name:"fp-NotifiedProgressItem_ProcessLabel"
                          [(item.process [@txt])]]
                      ; progress_bar ratio ]] ) ))
