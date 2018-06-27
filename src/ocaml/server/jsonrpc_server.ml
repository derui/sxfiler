module W = Websocket_cohttp_lwt
module J = Jsonrpc_ocaml_yojson

type t = {
  method_handler: J.Server.t;
  res_stream: J.Response.t Lwt_stream.t;
  res_writer: J.Response.t option -> unit;
  frame_stream: W.Frame.t Lwt_stream.t;
  frame_writer: W.Frame.t option -> unit;
  migemo: Migemocaml.Migemo.t;
}

let make ~migemo =
  let method_handler = J.Server.make () in
  let res_stream, res_writer = Lwt_stream.create () in
  let frame_stream, frame_writer = Lwt_stream.create () in
  {
    method_handler;
    res_stream;
    res_writer;
    frame_stream;
    frame_writer;
    migemo;
  }

let default_request_handler f writer =
  let open Websocket_cohttp_lwt in
  match f.Frame.opcode with
  | Frame.Opcode.Ping ->
    let f = Frame.create ~opcode:Frame.Opcode.Pong ~content:f.Frame.content () in
    Lwt.return @@ writer @@ Some f
  | Frame.Opcode.Close -> Lwt.fail Exit
  | _ as op -> Lwt_io.eprintf "Not implemented opcode: %s" (Frame.Opcode.to_string op)

(** handle response as JSON-RPC payload. This handler limits handling unfragment frames.  *)
let request_handler t frame_output_fn =
  let open Websocket_cohttp_lwt in
  Lwt_stream.iter_s (fun f ->
      match f.Frame.opcode with
      | Frame.Opcode.Text -> begin
          let json = Yojson.Basic.from_string f.Frame.content in
          match J.Request.of_json json with
          | Error _ ->
            Lwt.return @@ t.res_writer @@ Some J.Response.{
              result = None;
              id = None;
              error = Some J.Error.{
                  code = J.Types.Error_code.Parse_error;
                  message = J.Types.Error_code.(to_message Parse_error);
                  data = None;
                }
            }
          | Ok req ->
            let%lwt res =  J.Server.handle_request ~request:req t.method_handler in
            Lwt.return @@ t.res_writer @@ Some res
        end
      | _ -> default_request_handler f frame_output_fn
    ) t.frame_stream

(** Serve response sending loop *)
let serve_forever t frame_output_fn =
  Lwt.join [
    Lwt_stream.iter_s (fun res ->
        let json = J.Response.to_json res in
        let content = Yojson.Basic.to_string json in
        Lwt.wrap1 frame_output_fn @@ Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ())
      )
      t.res_stream;
    request_handler t frame_output_fn;
  ]
