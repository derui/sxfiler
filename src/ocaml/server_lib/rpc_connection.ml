(** Rpc_connection handle request and response on Websocket as Lwt stream.
    This module defines global connection to be able to use other module to send
    frame.
*)
open Rpc_connection_abbrev

(** Type of Rpc_connection. output_writer is created by Websocket_cohttp_lwt.upgrade_connection,
    and do not get stream of it.
*)
type t = {
  mutable output_writer: W.Frame.t option -> unit;
  input_stream: W.Frame.t Lwt_stream.t;
  input_writer: W.Frame.t option -> unit;
}

let connection : t option ref = ref None

(** [write_output t ~frame] writes a [frame] to peer on websocket connection.
    This function calls writer given with calling {!connect}.
*)
let write_output t ~frame = t.output_writer frame

(** [process_input t ~f] handles input of [t] on websocket connection with [f] *)
let process_input t ~f = Lwt_stream.iter_s f t.input_stream

(** [push_input t ~frame] writes a [frame] to peer on websocket connection. *)
let push_input t ~frame = t.input_writer frame

(** [with_conn f] execute [f] if connection already created. If does not exists connection,
    do nothing this.
*)
let with_conn f =
  match !connection with
  | None -> Lwt.return_unit
  | Some conn -> f conn

let make () =
  let input_stream, input_writer = Lwt_stream.create () in
  {
    output_writer = (fun _ -> ());
    input_stream;
    input_writer;
  }

(** [connect output_writer] connect to websocket with [output_writer].
    Connection is saved as global.
*)
let connect t output_writer =
  if Lwt_stream.is_closed t.input_stream then
    Lwt.return_unit
  else
    match !connection with
    | None ->
      connection := Some t;
      t.output_writer <- output_writer;
      Lwt.return_unit
    | Some t' ->
      let open Lwt in
      t'.input_writer None;
      let%lwt () = Lwt_stream.closed t'.input_stream in
      connection := Some t;
      t.output_writer <- output_writer;
      Lwt.return_unit

(** [disconnect ()] kills global connection. *)
let disconnect () =
  let open Lwt in
  match !connection with
  | None -> Lwt.return_unit
  | Some t ->
    t.input_writer None;
    Lwt_stream.closed t.input_stream
    >>= fun () -> connection := None; Lwt.return_unit

(** [default_input_handler t f] handles frame [f] with default behavior for Websocket. *)
let default_input_handler t f =
  let open Websocket_cohttp_lwt in
  match f.Frame.opcode with
  | Frame.Opcode.Ping ->
    let f = Frame.create ~opcode:Frame.Opcode.Pong ~content:f.Frame.content () in
    Lwt.return @@ t.output_writer @@ Some f
  | Frame.Opcode.Close -> Lwt.return_unit
  | _ as op -> Lwt_io.eprintf "Not implemented opcode: %s" (Frame.Opcode.to_string op)
