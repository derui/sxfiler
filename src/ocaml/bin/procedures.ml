open Sxfiler_server
module G = Sxfiler_server_gateway

let expose_key_map_procedures (module Dep : Dependencies.S) : (module Procedure.Spec) list =
  let module Get_gateway = G.Keymap.Get.Make (Dep.Usecase.Keymap_get) in
  let module Reload_gateway = G.Keymap.Reload.Make (Dep.Usecase.Keymap_reload) in
  [(module Proc_keymap.Get_spec (Get_gateway)); (module Proc_keymap.Reload_spec (Reload_gateway))]

let expose_filer_procedures (module Dep : Dependencies.S) : (module Procedure.Spec) list =
  let module Make_gateway = G.Filer.Make.Make (Dep.System) (Dep.Usecase.Filer_make) in
  let module Get_gateway = G.Filer.Get.Make (Dep.Usecase.Filer_get) in
  let module Move_parent_gateway = G.Filer.Move_parent.Make (Dep.Usecase.Filer_move_parent) in
  let module Enter_directory_gateway =
    G.Filer.Enter_directory.Make (Dep.Usecase.Filer_enter_directory) in
  let module Toggle_mark_gateway = G.Filer.Toggle_mark.Make (Dep.Usecase.Filer_toggle_mark) in
  let module Move_gateway = G.Filer.Move.Make (Dep.Usecase.Filer_move) in
  let module Copy_gateway = G.Filer.Copy.Make (Dep.Usecase.Filer_copy) in
  let module Delete_gateway = G.Filer.Delete.Make (Dep.Usecase.Filer_delete) in
  [ (module Proc_filer.Make_spec (Make_gateway))
  ; (module Proc_filer.Get_spec (Get_gateway))
  ; (module Proc_filer.Move_parent_spec (Move_parent_gateway))
  ; (module Proc_filer.Enter_directory_spec (Enter_directory_gateway))
  ; (module Proc_filer.Toggle_mark_spec (Toggle_mark_gateway))
  ; (module Proc_filer.Move_spec (Move_gateway))
  ; (module Proc_filer.Copy_spec (Copy_gateway))
  ; (module Proc_filer.Delete_spec (Delete_gateway)) ]

let expose_configuration_procedures (module Dep : Dependencies.S) : (module Procedure.Spec) list =
  let module S = Jsonrpc_yojson.Server in
  let module Gateway = G.Configuration.Get.Make (Dep.Usecase.Configuration_get) in
  [(module Proc_configuration.Get_spec (Gateway))]

let expose_completion_procedures (module Dep : Dependencies.S) : (module Procedure.Spec) list =
  let module S = Jsonrpc_yojson.Server in
  let module Setup_gateway = G.Completion.Setup.Make (Dep.Usecase.Completion_setup) in
  let module Read_gateway = G.Completion.Read.Make (Dep.Usecase.Completion_read) in
  [ (module Proc_completion.Setup_spec (Setup_gateway))
  ; (module Proc_completion.Read_spec (Read_gateway)) ]

let expose_task_procedures (module Dep : Dependencies.S) : (module Procedure.Spec) list =
  let module S = Jsonrpc_yojson.Server in
  let module Gateway = G.Task.Send_reply.Make (Dep.Usecase.Task_send_reply) in
  [(module Proc_task.Send_reply_spec (Gateway))]

let expose_all server (module Dep : Dependencies.S) =
  let procedures =
    List.concat
      [ expose_filer_procedures (module Dep)
      ; expose_key_map_procedures (module Dep)
      ; expose_configuration_procedures (module Dep)
      ; expose_completion_procedures (module Dep)
      ; expose_task_procedures (module Dep) ]
  in
  List.fold_left
    (fun server (module Spec : Procedure.Spec) ->
       Jsonrpc_server.expose server ~procedure:(module Procedure.Make (Spec)))
    server procedures
