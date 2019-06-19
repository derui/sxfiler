open Sxfiler_server
module W = Websocket
module J = Jsonrpc
module Jy = Jsonrpc_yojson

type t = {method_handler : Jy.Server.t}

let make () =
  let method_handler = Jy.Server.make () in
  {method_handler}

let expose t ~procedure =
  let module P = (val procedure : Procedure.S) in
  let _method = P.method_ and handler = P.handle in
  {method_handler = Jy.Server.expose ~_method ~handler t.method_handler}

let res_to_frame res =
  let json = Jy.Response.to_json res in
  let content = Yojson.Safe.to_string json in
  Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ())

(** handle response as JSON-RPC payload. This handler limits handling unfragment frames. *)
let request_handler t conn =
  let open Sxfiler_server_core in
  let module C = (val conn : Rpc_connection.Instance) in
  let conn = C.instance in
  C.Connection.process_input conn ~f:(fun f ->
      match f.W.Frame.opcode with
      | W.Frame.Opcode.Text -> (
          let json = Yojson.Safe.from_string f.W.Frame.content in
          match Jy.Request.of_json json with
          | Error _ ->
            let%lwt res =
              Lwt.return @@ res_to_frame
              @@ { Jy.Response.result = None
                 ; id = None
                 ; error = Some (Jy.Error.make J.Types.Error_code.Parse_error) }
            in
            Lwt.return @@ C.Connection.write_output conn ~frame:res
          | Ok req ->
            let%lwt res = Jy.Server.handle_request ~request:req t.method_handler in
            let%lwt frame = Lwt.return @@ res_to_frame @@ res in
            Lwt.return @@ C.Connection.write_output conn ~frame )
      | _ -> C.Connection.default_input_handler conn f )

(** Serve response sending loop *)
let serve_forever t conn = request_handler t conn
