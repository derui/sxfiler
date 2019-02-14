(** [Notification] provides types for notification that send to client. *)

(** Level of notification. *)
module Level = struct
  type t =
    | Info
    | Warning
    | Error

  let to_int = function Info -> 0 | Warning -> 1 | Error -> 2
  let of_int = function 0 -> Some Info | 1 -> Some Warning | 2 -> Some Error | _ -> None
end

(** [body] describes body of the notification. *)
type body =
  | Message of string
  | Progress of {process : string; current : float; targeted : float}

(** Identifier of the notification. Each notifications has global unique identifier.  *)
type id = Uuidm.t

type t =
  { id : id
  ; level : Level.t
  ; body : body }

let make ~id ~level ~body = {id; level; body}

module type Factory = sig
  val create : level:Level.t -> body:body -> t
  (** [create ~level ~body] create a instance of [t]. *)
end
