(** Interface for completion service *)
open Abbrevs

(** {!S} provides interface to call RPC providing by server.  *)
module type S = sig

  (** [setup params] calls setup RPC with [params].  *)
  val setup: E.Completion.Setup.params -> E.Completion.Setup.result Lwt.t

  (** [read params] calls read RPC with [params].  *)
  val read: E.Completion.Read.params -> E.Completion.Read.result Lwt.t
end
