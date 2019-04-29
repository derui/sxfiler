(** level of notification. *)
module Level : sig
  type t =
    | Info
    | Warning
    | Error
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Notification.Level.t
end

module Body : sig
  type t =
    | Message of string
    | Progress of {process : string; current : float; targeted : float}
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Notification.body
end

(** the type that is JSON friendly for {!Sxfiler_domain.Notification.t} *)
type t =
  { id : string
  ; level : Level.t
  ; body : Body.t }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Notification.t
