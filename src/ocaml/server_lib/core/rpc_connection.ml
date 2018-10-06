(** Rpc_connection handle request and response on Websocket as Lwt stream.
    This module defines global connection to be able to use other module to send
    frame.
*)
open Rpc_connection_abbrev

include Rpc_connection_intf

module Impl = struct
  (** Type of Rpc_connection. output_writer is created by Websocket_cohttp_lwt.upgrade_connection,
      and do not get stream of it.
  *)
  type t =
    { mutable output_writer : W.Frame.t option -> unit
    ; input_stream : W.Frame.t Lwt_stream.t
    ; input_writer : W.Frame.t option -> unit }

  let write_output t ~frame = t.output_writer frame
  let process_input t ~f = Lwt_stream.iter_s f t.input_stream
  let push_input t ~frame = t.input_writer frame
  let tags = Logger.Tags.module_lib ["rpc_connection"]

  let connect t output_writer =
    if Lwt_stream.is_closed t.input_stream then
      let%lwt () =
        Logs_lwt.warn @@ fun m -> m ~tags "Detected re-connect with disconnected connection"
      in
      Lwt.return_unit
    else
      let%lwt () = Logs_lwt.info @@ fun m -> m ~tags "Connection connected with Websocket" in
      t.output_writer <- output_writer ;
      Lwt.return_unit

  let disconnect t =
    if Lwt_stream.is_closed t.input_stream then
      let%lwt () =
        Logs_lwt.warn @@ fun m -> m ~tags "Detected disconnect with disconnected connection"
      in
      Lwt.return_unit
    else (
      t.input_writer None ;
      t.output_writer <- (fun _ -> ()) ;
      let%lwt () = Logs_lwt.info @@ fun m -> m ~tags "Connection disconnected" in
      Lwt_stream.closed t.input_stream )

  let is_closed t = Lwt_stream.is_closed t.input_stream

  (** [default_input_handler t f] handles frame [f] with default behavior for Websocket. *)
  let default_input_handler t f =
    let open Websocket_cohttp_lwt in
    match f.Frame.opcode with
    | Frame.Opcode.Ping ->
      let f = Frame.create ~opcode:Frame.Opcode.Pong ~content:f.Frame.content () in
      Lwt.return @@ t.output_writer @@ Some f
    | Frame.Opcode.Close -> disconnect t
    | _ as op ->
      Logs_lwt.err @@ fun m -> m ~tags "Not implemented opcode: %s" (Frame.Opcode.to_string op)
end

let make () =
  let input_stream, input_writer = Lwt_stream.create () in
  let t = {Impl.output_writer = (fun _ -> ()); input_stream; input_writer} in
  ( module struct
    module Connection = Impl

    let instance = t
  end
  : Instance )
