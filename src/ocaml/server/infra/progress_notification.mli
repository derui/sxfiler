(** [Notification] provides types for notification that send to client. *)

type body =
  { process : string
  ; current : float
  ; targeted : float }
(** [body] describes body of the notification. *)

type id = Uuidm.t
(** Identifier of the notification. Each notifications has global unique identifier. *)

type t = private
  { id : id
  ; body : body }

val make : id:id -> body:body -> t
(** [make ~id ~body] is as constructor of [t] *)

val update_progress : current:float -> targeted:float -> t -> t
(** [update_progress ~current ~targeted t] get the progress updated with [current] and [target] *)

(** Json representation *)
module Json : sig
  module Body : sig
    type t =
      { process : string
      ; current : float
      ; targeted : float }
    [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  type t =
    { id : string
    ; body : Body.t }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  (** the type that is JSON friendly for {!Sxfiler_domain.Notification.t} *)
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
