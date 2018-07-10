(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module T = Sxfiler_types
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module Jr = Jsonrpc_ocaml_jsoo

module Workspace_update = struct
  include R.Notification.Workspace_update
  open Rj.Notification.Workspace_update

  let handler store req =
    match req.Jr.Request.params with
    | None -> Lwt.return_unit
    | Some params ->
      let params = params_of_json @@ Js.Unsafe.coerce params in

      let state = Store.get store in
      Store.update store @@ State.with_stack state ~name:params.name ~f:(fun stack ->
          let viewer = Types.Viewer.(File_tree {
              File_tree.snapshot = params.workspace.T.Workspace.current;
              selected_item_index = 0;
            }) in
          let stack = match stack with
            | None -> Types.Viewer_stack.empty ()
            | Some stack -> stack
          in
          Types.(Viewer_stack.push stack ~v:(Viewer_state.make viewer))
        );
      Lwt.return_unit

end


let expose ~store server =
  Rpc.Notification_server.expose ~_method:Workspace_update.name ~handler:(Workspace_update.handler store) server
