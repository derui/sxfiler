(** This module implements {!Sxfiler_domain.Notification_service.S} *)

module C = Sxfiler_server_core
module Jy = Jsonrpc_yojson
module Tr = Sxfiler_server_translator
module W = Websocket
include Notification_service_intf

module Make (Conn : C.Rpc_connection.Instance) : S = struct
  let send ~typ v =
    let _method = typ.to_method v and params = typ.to_json v in
    let content =
      Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
      |> Yojson.Safe.to_string
    in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return Conn.(Connection.write_output instance ~frame)
end
