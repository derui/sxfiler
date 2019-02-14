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

(** Identifier of the notification. Each notifications has global unique identifier.  *)
type id = string

type t =
  { id : id
  ; level : Level.t
  ; body : Body.t }

(** The module to translate between {!t} and {!Sxfiler_domain.Notification.t} *)
module Domain : Core.Translator with type t = t and type target = Sxfiler_domain.Notification.t

(** The module to translate between {!t} and {!Yojson.Safe.t} *)
module Json : Core.Translator with type t = t and type target = Yojson.Safe.t
