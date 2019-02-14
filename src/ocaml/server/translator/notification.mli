(** level of notification. *)
module Level : sig
  type t =
    | Info
    | Warning
    | Error
end

module Body : sig
  type t =
    | Message of string
    | Progress of {process : string; current : float; targeted : float}
end

(** the type that is JSON friendly for {!Sxfiler_domain.Notification.t} *)
type t =
  { id : string
  ; level : Level.t
  ; body : Body.t }
[@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Notification.t
