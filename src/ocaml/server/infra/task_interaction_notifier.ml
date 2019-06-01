(** This module implements {!Sxfiler_domain.task_interaction.Notifier.S} *)

module C = Sxfiler_server_core
module D = Sxfiler_domain
module Jy = Jsonrpc_yojson
module Tr = Sxfiler_server_translator
module W = Websocket

module Make (Conn : C.Rpc_connection.Instance) : D.Task_notifier.S = struct
  let need_interaction ~suggestions ~node_name task_id =
    let t' = {D.Task_interaction.Suggestion.task_id; node_name; suggestions} in
    let _method = "notification/task/needInteraction" in
    let params = Tr.Task_interaction.Suggestion.(of_domain t' |> to_json) in
    let content =
      Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
      |> Yojson.Safe.to_string
    in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return Conn.(Connection.write_output instance ~frame)

  let finished task_id =
    let _method = "notification/task/finished" in
    let params = `String (D.Task_types.show_id task_id) in
    let content =
      Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
      |> Yojson.Safe.to_string
    in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return Conn.(Connection.write_output instance ~frame)
end
