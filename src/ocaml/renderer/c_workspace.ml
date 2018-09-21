(** Layout container for viewer stack. *)
open Sxfiler_core

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let file_list_container ~key store =
  let filer = S.File_list.Store.get @@ S.App.State.file_list store in
  let ws = S.Workspace.Store.get @@ S.App.State.workspace store in
  if S.Workspace.State.current_mode ws = Preview then R.empty ()
  else
    [%c
      P_file_list_container.t ~key
        ~props:
          (object%js
            val filerState = filer

            val focused = S.Workspace.State.current_mode ws = File_tree
          end)]

let previewer_container ~key store =
  let open Fun in
  let plan = (S.App.State.command %> S.Command.Store.get) store in
  let ws = (S.App.State.workspace %> S.Workspace.Store.get) store in
  match plan.S.Command.State.plan with
  | None -> R.empty ()
  | Some plan ->
    if S.Workspace.State.current_mode ws <> C.Types.Mode.Preview then R.empty ()
    else
      [%c
        P_plan_previewer.t ~key
          ~props:
            (object%js
              val plan = plan
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
         ~initial_state:(fun _ _ -> object%js end)
         ~initial_custom:(fun _ _ ->
             let refs = R.Ref_table.create () in
             R.Ref_table.define ~key:container_key refs ;
             object%js
               val refs = refs
             end )
         ~component_did_mount:(fun this ->
             let open Option in
             ignore (R.Ref_table.find ~key:container_key this##.custom##.refs >|= fun e -> e##focus)
           )
         ~component_did_update:(fun this _ _ ->
             let module L = (val this##.props##.locator : Locator.S) in
             let store = S.App.Store.get L.store in
             let ws = S.Workspace.Store.get @@ S.App.State.workspace store in
             let focused = S.Workspace.State.current_mode ws <> Complete in
             let open Option in
             if focused then
               ignore (R.Ref_table.find ~key:container_key this##.custom##.refs >|= fun e -> e##focus)
             else () )
         (fun this ->
            let module L = (val this##.props##.locator : Locator.S) in
            let store = S.App.Store.get L.store in
            let module C = T.Configuration in
            let class_name = Classnames.to_string [("fp-Workspace", true)] in
            [%e
              div ~key:"layout" ~class_name
                ~_ref:(R.Ref_table.use ~key:container_key this##.custom##.refs)
                ~others:
                  (object%js
                    val tabIndex = Js.string "0"
                  end)
                [file_list_container ~key:"filer" store; previewer_container ~key:"preview" store]]
         ))
