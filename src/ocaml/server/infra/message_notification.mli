(** [Notification] provides types for notification that send to client. *)

(** Level of notification. *)
type level =
  | Info
  | Warning
  | Error

type body = string
(** [body] describes body of the notification. *)

type id = Uuidm.t
(** Identifier of the notification. Each notifications has global unique identifier. *)

type t = private
  { id : id
  ; level : level
  ; body : body }

val make : id:id -> level:level -> body:body -> t
(** [make ~id ~level ~body] is as constructor of [t] *)

(** Json representation *)
module Json : sig
  (** level of notification. *)
  module Level : sig
    type t =
      | Info [@key "info"]
      | Warning [@key "warning"]
      | Error [@key "error"]
    [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module Body : sig
    type t = string [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  type t =
    { id : string
    ; level : Level.t
    ; body : Body.t }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  (** the type that is JSON friendly for {!Sxfiler_domain.Notification.t} *)
end

(** Conversion between domain and JSON representation *)
module Conv : sig
  module Level : sig
    include
      Sxfiler_server_translator.Core.Domain_translator
        with type t := Json.Level.t
         and type domain := level
  end

  module Body : sig
    include
      Sxfiler_server_translator.Core.Domain_translator
        with type t := Json.Body.t
         and type domain := body
  end

  include
    Sxfiler_server_translator.Core.Domain_translator with type t := Json.t and type domain := t
end

val notification_typ : t Notification_service.typ
(** conversion configuration for notification *)
