(** This module implements {!Sxfiler_domain.task_interaction.Notifier.S} *)

module C = Sxfiler_server_core
module D = Sxfiler_domain
module Jy = Jsonrpc_yojson
module Tr = Sxfiler_server_translator
module W = Websocket

module Make (Conn : C.Rpc_connection.Instance) (F : D.Task_interaction.Factory.S) :
  D.Task_interaction.Notifier.S = struct
  let need_interaction ~accept task_id =
    let t' = F.create ~task_id ~accept_interactions:accept in
    let _method = "notification/taskInteraction" in
    let params = Tr.Task_interaction.of_domain t' |> Tr.Task_interaction.to_json in
    let content =
      Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
      |> Yojson.Safe.to_string
    in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return Conn.(Connection.write_output instance ~frame)

  let finished task_id =
    let _method = "notification/taskFinished" in
    let params = `String (D.Task.show_id task_id) in
    let content =
      Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
      |> Yojson.Safe.to_string
    in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return Conn.(Connection.write_output instance ~frame)
end
