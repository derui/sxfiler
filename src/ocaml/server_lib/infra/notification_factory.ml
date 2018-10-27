(** A implementation for {Sxfiler_domain.Notification.Factory} signature. *)

module T = Sxfiler_domain

let id_gen = Uuidm.v4_gen (Random.get_state ())

let create ~level ~body =
  let id = id_gen () in
  T.Notification.make ~id ~level ~body
