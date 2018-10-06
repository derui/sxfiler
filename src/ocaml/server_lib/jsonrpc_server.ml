open Sxfiler_server_core
module W = Websocket_cohttp_lwt
module J = Jsonrpc_ocaml_yojson

type t = {method_handler : J.Server.t}

let make () =
  let method_handler = J.Server.make () in
  {method_handler}

let expose t ~operation =
  let module S = (val operation : Operation_intf.S) in
  {method_handler = S.expose t.method_handler}

let res_to_frame res =
  let json = J.Response.to_json res in
  let content = Yojson.Safe.to_string json in
  Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ())

(** handle response as JSON-RPC payload. This handler limits handling unfragment frames.  *)
let request_handler t conn =
  let open Websocket_cohttp_lwt in
  let module C = (val conn : Rpc_connection.Instance) in
  let conn = C.instance in
  C.Connection.process_input conn ~f:(fun f ->
      match f.Frame.opcode with
      | Frame.Opcode.Text -> (
          let json = Yojson.Safe.from_string f.Frame.content in
          match J.Request.of_json json with
          | Error _ ->
            let%lwt res =
              Lwt.return @@ res_to_frame
              @@ { J.Response.result = None
                 ; id = None
                 ; error = Some {J.Error.code = J.Types.Error_code.Parse_error; data = None} }
            in
            Lwt.return @@ C.Connection.write_output conn ~frame:res
          | Ok req ->
            let%lwt res = J.Server.handle_request ~request:req t.method_handler in
            let%lwt frame = Lwt.return @@ res_to_frame @@ res in
            Lwt.return @@ C.Connection.write_output conn ~frame )
      | _ -> C.Connection.default_input_handler conn f )

(** Serve response sending loop *)
let serve_forever t conn = request_handler t conn
