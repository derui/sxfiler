open Abbrev

type response = (G.Service.Response.t * F.event list) Lwt.t

type request = G.Service.Request.t

type t = request -> response
(** endpoint signature *)

type mapping = G.Service.Command.t * t
(** mapping of command and endpoint *)

val with_request :
  (Pb.Reader.t -> 'a Pb.Result.t) * ('b -> Pb.Writer.t) ->
  request ->
  f:('a -> ('b * F.event list, Endpoint_error.t) result Lwt.t) ->
  response
(** [with_request (from_proto, to_proto) request ~f] helper function to wrap up common process for RPC request and
    response. *)
