(** This module defines procedures to make endpoint with service function and RPC connection. *)

open Abbrev
open Sxfiler_core
module S = Sxfiler_dependency

module L = (val I.Logger.make [ "rpc_server" ])

type 'a response = (G.Service.Response.t * F.event list, 'a) S.t

type request = G.Service.Request.t

type 'a t = request -> 'a response

type 'a mapping = G.Service.Command.t * 'a t

let with_request (from_proto, to_proto) request ~f =
  let id = request.G.Service.Request.id in
  let request_payload = request.G.Service.Request.payload |> Bytes.to_string |> Pb.Reader.create |> from_proto in
  match request_payload with
  | Error _    ->
      S.return
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
      let open S.Infix in
      S.catch
        (fun () ->
          let* v = f payload in
          match v with
          | Ok (payload, events) ->
              S.return
                ( {
                    G.Service.Response.id;
                    status = G.Service.Status.SUCCESS;
                    payload = to_proto payload |> Pb.Writer.contents |> Bytes.of_string;
                    error = None;
                  },
                  events )
          | Error error          ->
              S.return
                ( {
                    G.Service.Response.id;
                    status = G.Service.Status.COMMAND_FAILED;
                    payload = Bytes.empty;
                    error = Some (Endpoint_error.to_endpoint_error error);
                  },
                  [] ))
        (fun e ->
          L.err (fun m -> m "Raise error: %s" (Printexc.to_string e)) |> Lwt.ignore_result;
          ( {
              G.Service.Response.id;
              status = G.Service.Status.COMMAND_FAILED;
              payload = Bytes.empty;
              error = Some Endpoint_error.(to_endpoint_error @@ unknown @@ Printf.sprintf "raise unknown error");
            },
            [] ))
