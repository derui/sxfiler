(** Notification_service provides notification what current progression of work.

    Each notification do not have identifier, but have group identifier to collect
    same group in external service.
*)

type group = string
type notification = Progress of float

(** [notification] can send to subscriber with service. *)
type t =
  { group : group
  ; notification : notification }

let make ~group ~notification = {group; notification}

module type S = sig
  val send : t -> unit Lwt.t
  (** [send t] send a notification [t] to external service. *)
end
