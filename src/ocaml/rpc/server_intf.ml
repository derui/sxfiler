module type S = sig
  type t

  val start : t -> mappings:Endpoint.mapping list -> post_event:Event_handler.publish -> unit Lwt.t
  (** [start t ~mappings] start handling messages. Please use as waiter returned computation from this function. *)

  val stop : t -> unit
  (** [stop t] stop handling messages. This function returns immediately, but you should wait to return value from thunk
      returned from [start]. *)
end

(** Signature of Server instance. *)
module type Instance = sig
  module Server : S

  val instance : Server.t
end
