(** [Notification] provides types for notification that send to client. *)

(** Level of notification. *)
module Level : sig
  type t =
    | Info
    | Warning
    | Error

  val to_int : t -> int
  val of_int : int -> t option
end

(** [body] describes body of the notification. *)
type body =
  | Message of string
  | Progress of {process : string; current : float; targeted : float}

(** Identifier of the notification. Each notifications has global unique identifier.  *)
type id = Uuidm.t

type t = private
  { id : id
  ; level : Level.t
  ; body : body }

val make : id:id -> level:Level.t -> body:body -> t
(** [make ~id ~level ~body] is as constructor of [t]  *)

module type Factory = sig
  val create : level:Level.t -> body:body -> t
  (** [create ~level ~body] create a instance of [t]. *)
end
