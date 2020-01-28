open Abbrev
(** This module provides interface and implementations for RPC client *)

type ('a, 'b) client_function = ('a -> Pb.Writer.t) * (Pb.Reader.t -> 'b Pb.Result.t)

type error = [ `Pb_error of Pb.Result.error ]

module type S = sig
  type t

  val call : t -> ('a, 'b) Client_command.t -> 'a -> ('b, error) result Lwt.t
  (** [send client_function command payload] call RPC of [command] with [payload]. *)
end

(** Signature of client instance *)
module type Instance = sig
  module Client : S

  val instance : Client.t
end
