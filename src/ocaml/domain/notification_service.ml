(** Notification_service provides notification that contains simple message or
    progression of work.
*)

module type S = sig
  val send : Notification.t -> unit Lwt.t
  (** [send t] send a notification [t] to external service. *)
end
