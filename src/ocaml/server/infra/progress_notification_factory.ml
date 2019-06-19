(** A implementation for {!module:Sxfiler_server_core.Notification.Factory} signature. *)

module T = Progress_notification
module D = Sxfiler_domain

module type S = sig
  val create : body:T.body -> T.t
  (** [create ~body] create a instance of [t]. *)
end

module Make (G : D.Id_generator_intf.Gen_random with type id = Uuidm.t) : S = struct
  let create ~body =
    let id = G.generate () in
    T.make ~id ~body
end
