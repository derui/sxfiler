(** [Notification] provides types for notification that send to client. *)

(** [body] describes body of the notification. *)
type body =
  { process : string
  ; current : float
  ; targeted : float }

(** Identifier of the notification. Each notifications has global unique identifier. *)
type id = Uuidm.t

type t = private
  { id : id
  ; body : body }

val make : id:id -> body:body -> t
(** [make ~id ~body] is as constructor of [t] *)

(** Json representation *)
module Json : sig
  module Body : sig
    type t =
      { process : string
      ; current : float
      ; targeted : float }
    [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  (** the type that is JSON friendly for {!Sxfiler_domain.Notification.t} *)
  type t =
    { id : string
    ; body : Body.t }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
end

(** Conversion between domain and JSON representation *)
module Conv : sig
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
