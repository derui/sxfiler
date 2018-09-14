(** Layout container for viewer stack. *)
open Sxfiler_core

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let file_list_container ~key store =
  let filer = S.File_list.Store.get @@ S.App.State.file_list store in
  let ws = S.Workspace.Store.get @@ S.App.State.workspace store in
  [%c
    P_file_list_viewer.t ~key
      ~props:
        (object%js
          val filerState = filer

          val focused = S.Workspace.State.current_mode ws = File_tree
        end)]

let container_key = "container"

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method locator : (module Locator.S) Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~constructor:(fun this _ -> this##.nodes := Jstable.create ())
         ~initial_state:(fun _ _ -> object%js end)
         ~initial_custom:(fun _ _ -> object%js end)
         ~component_did_mount:(fun this ->
             let open Option in
             ignore (R.Ref_table.find ~key:container_key this##.nodes >|= fun e -> e##focus) )
         ~component_did_update:(fun this _ _ ->
             let module L = (val this##.props##.locator : Locator.S) in
             let store = S.App.Store.get L.store in
             let ws = S.Workspace.Store.get @@ S.App.State.workspace store in
             let focused = S.Workspace.State.current_mode ws = File_tree in
             let open Option in
             if focused then
               ignore (R.Ref_table.find ~key:container_key this##.nodes >|= fun e -> e##focus)
             else () )
         (fun this ->
            let module L = (val this##.props##.locator : Locator.S) in
            let store = S.App.Store.get L.store in
            let module C = T.Configuration in
            let class_name = Classnames.to_string [("fp-Workspace", true)] in
            [%e
              div ~key:"layout" ~class_name
                ~_ref:(fun e -> R.Ref_table.add ~key:container_key ~value:e this##.nodes)
                ~others:
                  (object%js
                    val tabIndex = Js.string "0"
                  end)
                [file_list_container ~key:"filer" store]] ))
