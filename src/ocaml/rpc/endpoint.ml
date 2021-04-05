(** This module defines procedures to make endpoint with service function and RPC connection. *)

open Abbrev
open Sxfiler_core
module S = Sxfiler_dependency

module L = (val I.Logger.make [ "rpc_server" ])

type response = (G.Service.Response.t * F.event list) Lwt.t

type request = G.Service.Request.t

type t = request -> response

type mapping = G.Service.Command.t * t

let with_request (from_proto, to_proto) request ~f =
  let id = request.G.Service.Request.id in
  let request_payload = request.G.Service.Request.payload |> Bytes.to_string |> Pb.Reader.create |> from_proto in
  match request_payload with
  | Error _    ->
      Lwt.return
        ( {
            G.Service.Response.id;
            status = G.Service.Status.INVALID_REQUEST_PAYLOAD;
            payload = Bytes.empty;
            error =
              Some
                {
                  G.Service.Error.status = -1;
                  error_message = Printf.sprintf "Invalid payload for %s" & G.Service.Command.show request.command;
                  details = [];
                };
          },
          [] )
  | Ok payload ->
      Lwt.catch
        (fun () ->
          match%lwt f payload with
          | Ok (payload, events) ->
              Lwt.return
                ( {
                    G.Service.Response.id;
                    status = G.Service.Status.SUCCESS;
                    payload = to_proto payload |> Pb.Writer.contents |> Bytes.of_string;
                    error = None;
                  },
                  events )
          | Error error          ->
              Lwt.return
                ( {
                    G.Service.Response.id;
                    status = G.Service.Status.COMMAND_FAILED;
                    payload = Bytes.empty;
                    error = Some (Endpoint_error.to_endpoint_error error);
                  },
                  [] ))
        (fun e ->
          L.err (fun m -> m "Raise error: %s" (Printexc.to_string e)) |> Lwt.ignore_result;
          Lwt.return
            ( {
                G.Service.Response.id;
                status = G.Service.Status.COMMAND_FAILED;
                payload = Bytes.empty;
                error = Some Endpoint_error.(to_endpoint_error @@ unknown @@ Printf.sprintf "raise unknown error");
              },
              [] ))
