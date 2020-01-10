open Sxfiler_server
module W = Websocket
module J = Jsonrpc
module Jy = Jsonrpc_yojson

module Server =
  Jy.Server.Make
    (struct
      type t = Yojson.Safe.t
    end)
    (struct
      let serialize v =
        match Jy.Request.of_json v with Ok v -> Ok v | Error _ -> Error "can not parse"
    end)
    (struct
      let deserialize = Jy.Response.to_json
    end)

type t = { method_handler : Server.t }

let make () =
  let method_handler = Server.make () in
  { method_handler }

let expose t ~procedure =
  let module P = (val procedure : Procedure.S) in
  let _method = P.method_ and handler = P.handle in
  { method_handler = Server.expose ~_method ~handler t.method_handler }

let res_to_frame json =
  let content = Yojson.Safe.to_string json in
  Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ())

(** handle response as JSON-RPC payload. This handler limits handling unfragment frames. *)
let request_handler t conn =
  let open Sxfiler_server_core in
  let module C = (val conn : Rpc_connection.Instance) in
  let conn = C.instance in
  C.Connection.process_input conn ~f:(fun f ->
      match f.W.Frame.opcode with
      | W.Frame.Opcode.Text ->
          let json = Yojson.Safe.from_string f.W.Frame.content in
          let%lwt res = Server.handle_request ~request:json t.method_handler in
          let%lwt frame = Lwt.return @@ res_to_frame @@ res in
          Lwt.return @@ C.Connection.write_output conn ~frame
      | _ -> C.Connection.default_input_handler conn f)

(** Serve response sending loop *)
let serve_forever t conn = request_handler t conn
