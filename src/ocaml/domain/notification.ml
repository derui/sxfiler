(** [Notification] provides types for notification that send to client. *)

(** Level of notification. *)
type level =
  | Info
  | Warning
  | Error

(** [body] describes body of the notification. *)
type body =
  | OneShot of {message : string; level : level}
  | Progress of {process : string; level : level; current : float; targeted : float}

(** Identifier of the notification. Each notifications has global unique identifier.  *)
type id = Uuidm.t

type t =
  { id : id
  ; body : body }

let make ~id ~body = {id; body}

module type Factory = sig
  val create : body -> t
  (** [create body] create a instance of [t]. *)
end
