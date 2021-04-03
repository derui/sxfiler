open Abbrev

type 'a response = (G.Service.Response.t * F.event list, 'a) S.t

type request = G.Service.Request.t

type 'a t = request -> 'a response
(** endpoint signature *)

type 'a mapping = G.Service.Command.t * 'a t
(** mapping of command and endpoint *)

val with_request :
  (Pb.Reader.t -> 'a Pb.Result.t) * ('b -> Pb.Writer.t) ->
  request ->
  f:('a -> (('b * F.event list, Endpoint_error.t) result, 'dependency) S.t) ->
  'dependency response
(** [with_request (from_proto, to_proto) request ~f] helper function to wrap up common process for RPC request and
    response. *)
