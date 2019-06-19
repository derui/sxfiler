(** A implementation for {!module:Sxfiler_server_core.Notification.Factory} signature. *)

module T = Message_notification
module D = Sxfiler_domain

module type S = sig
  val create : level:T.level -> body:T.body -> T.t
  (** [create ~level ~body] create a instance of [t]. *)
end

module Make (G : D.Id_generator_intf.Gen_random with type id = Uuidm.t) : S = struct
  let create ~level ~body =
    let id = G.generate () in
    T.make ~id ~level ~body
end
