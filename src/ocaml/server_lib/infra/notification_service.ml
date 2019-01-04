(** This module implements {!Sxfiler_domain.Notification_service.S} *)

module C = Sxfiler_server_core
module D = Sxfiler_domain
module Jy = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator
module W = Websocket_cohttp_lwt

type accepter = Jy.Request.t
type t = {mutable accepter : accepter}

let make (module Conn : C.Rpc_connection.Instance) =
  ( module struct
    let send t =
      let _method =
        match t.D.Notification.body with
        | D.Notification.OneShot _ -> "notification/message"
        | D.Notification.Progress _ -> "notification/progress"
      in
      let params = Tr.Notification.of_domain t |> Tr.Notification.to_yojson in
      let content =
        Jy.Request.to_json {Jy.Request._method; params = Some params; id = None}
        |> Yojson.Safe.to_string
      in
      let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
      Lwt.return Conn.(Connection.write_output instance ~frame)
  end
  : D.Notification_service.S )