(** [Notification] provides types for notification that send to client. *)

(** [body] describes body of the notification. *)
type body =
  { process : string
  ; current : float
  ; targeted : float }

(** Identifier of the notification. Each notifications has global unique identifier. *)
type id = Uuidm.t

type t =
  { id : id
  ; body : body }

let make ~id ~body = {id; body}

module Json = struct
  (** body describes body of the notification. *)
  module Body = struct
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
end

module Conv = struct
  module Body = struct
    let of_domain v = {Json.Body.process = v.process; current = v.current; targeted = v.targeted}
    let to_domain v = {process = v.Json.Body.process; current = v.current; targeted = v.targeted}
  end

  let of_domain t = {Json.id = Uuidm.to_string t.id; body = Body.of_domain t.body}

  let to_domain t =
    let open Sxfiler_core in
    let body = Body.to_domain t.Json.body in
    make ~id:(Uuidm.of_string t.id |> Option.get_exn) ~body
end

let notification_typ : t Notification_service.typ =
  let open Sxfiler_core.Fun in
  {to_method = (fun _ -> "notification/progress"); to_json = Conv.of_domain %> Json.to_json}
