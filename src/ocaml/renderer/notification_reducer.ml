(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module T = Sxfiler_types
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module Jr = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core

module Scanner_update = struct
  include R.Notification.Scanner_update
  open Rj.Notification.Scanner_update

  let handler with_store req =
    match req.Jr.Request.params with
    | None -> Lwt.return_unit
    | Some params ->
      let params = params_of_json @@ Js.Unsafe.coerce params in

      with_store (fun store_group ->
          let store = C.Store_group.get store_group ~tag:Store.viewer_stacks in
          let module S = Store.Viewer_stacks_store in

          let viewer = C.Types.(Viewer_state.make @@ Viewer.(File_tree {
              File_tree.scanner = params.scanner;
              selected_item_index = 0;
            })) in
          let store = S.update store (C.Message.Update_viewer_stack (params.name, viewer)) in
          C.Store_group.update store_group ~tag:Store.viewer_stacks ~v:store
        );
      Lwt.return_unit
end


let expose ~with_store server =
  Rpc.Notification_server.expose ~_method:Scanner_update.name ~handler:(Scanner_update.handler with_store) server
