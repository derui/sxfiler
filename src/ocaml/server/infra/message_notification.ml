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

type t =
  { id : id
  ; level : level
  ; body : body }

let make ~id ~level ~body = {id; level; body}

module Json = struct
  (** level of notification. *)
  module Level = struct
    type t =
      | Info [@name "info"]
          | Warning [@name "warning"]
          | Error [@name "error"]
    [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  (** body describes body of the notification. *)
  module Body = struct
    type t = string [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
  end

  type t =
    { id : string
    ; level : Level.t
    ; body : Body.t }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
end

module Conv = struct
  module Level = struct
    let of_domain = function Info -> Json.Level.Info | Warning -> Warning | Error -> Error
    let to_domain = function Json.Level.Info -> Info | Warning -> Warning | Error -> Error
  end

  module Body = struct
    let of_domain t = t
    let to_domain t = t
  end

  let of_domain t =
    let level = Level.of_domain t.level in
    {Json.id = Uuidm.to_string t.id; body = Body.of_domain t.body; level}

  let to_domain t =
    let open Sxfiler_core in
    let level = Level.to_domain t.Json.level in
    let body = Body.to_domain t.body in
    make ~id:(Uuidm.of_string t.id |> Option.get_exn) ~body ~level
end

let notification_typ : t Notification_service.typ =
  let open Sxfiler_core.Fun in
  {to_method = (fun _ -> "notification/message"); to_json = Conv.of_domain %> Json.to_json}
