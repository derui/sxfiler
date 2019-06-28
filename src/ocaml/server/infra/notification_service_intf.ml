(** Notification_service provides notification that contains simple message or progression of work. *)

type 'a typ =
  { to_method : 'a -> string
  ; to_json : 'a -> Yojson.Safe.t }
(** setting to send notification with a value *)

module type S = sig
  val send : typ:'a typ -> 'a -> unit Lwt.t
  (** [send ~typ v] send a notification with [v] to external service. *)
end
