open Sxfiler_core
module D = Sxfiler_domain.Notification

module Type = struct
  (** level of notification. *)
  module Level = struct
    type t =
      | Info [@name "info"]
          | Warning [@name "warning"]
          | Error [@name "error"]
    [@@deriving yojson]
  end

  (** body describes body of the notification. *)
  module Body = struct
    type t =
      | Message of string
      | Progress of {process : string; current : float; targeted : float}

    let of_domain = function
      | D.Message message -> Message message
      | Progress v -> Progress {process = v.process; current = v.current; targeted = v.targeted}

    let to_domain = function
      | Message message -> D.Message message
      | Progress v -> Progress {process = v.process; current = v.current; targeted = v.targeted}

    let to_yojson = function
      | Message body -> [("type", `String "message"); ("body", `String body)]
      | Progress body ->
        [ ("type", `String "progress")
        ; ( "body"
          , `Assoc
              [ ("targeted", `Float body.targeted)
              ; ("current", `Float body.current)
              ; ("process", `String body.process) ] ) ]

    let of_yojson js =
      let open Yojson.Safe.Util in
      try
        let typ = js |> member "type" |> to_string and body = js |> member "body" in
        match typ with
        | "message" -> Ok (Message (to_string body))
        | "progress" ->
          let targeted = body |> member "targeted" |> to_float
          and current = body |> member "current" |> to_float
          and process = body |> member "process" |> to_string in
          Ok (Progress {targeted; current; process})
        | _ -> Error (Printf.sprintf "Unknown type: %s" typ)
      with Type_error (s, _) -> Error s
  end

  (** Identifier of the notification. Each notifications has global unique identifier.  *)
  type id = string [@@deriving yojson]

  type t =
    { id : id
    ; level : Level.t
    ; body : Body.t }
end

module Domain = struct
  type t = Type.t
  type target = Sxfiler_domain.Notification.t

  let of_target t =
    let level =
      match t.D.level with
      | Info -> Type.Level.Info
      | Warning -> Type.Level.Warning
      | Error -> Type.Level.Error
    in
    Ok {Type.id = Uuidm.to_string t.D.id; body = Type.Body.of_domain t.body; level}

  let to_target t =
    let level =
      match t.Type.level with
      | Info -> D.Level.Info
      | Warning -> D.Level.Warning
      | Error -> D.Level.Error
    in
    let body = Type.Body.to_domain t.Type.body in
    D.make ~id:(Uuidm.of_string t.Type.id |> Option.get_exn) ~body ~level
end

module Json = struct
  type t = Type.t
  type target = Yojson.Safe.t

  let to_target t =
    let common = [("id", `String t.Type.id); ("level", Type.Level.to_yojson t.level)] in
    let assoc = Type.Body.to_yojson t.body @ common in
    `Assoc assoc

  let of_target js =
    let open Yojson.Safe.Util in
    try
      let id = js |> member "id" |> to_string
      and level = js |> member "level"
      and body = js |> member "body" in
      let open Result in
      Type.Level.of_yojson level
      >>= fun level -> Type.Body.of_yojson body >>= fun body -> Ok {Type.id; level; body}
    with Type_error (s, _) -> Error s
end

include Type
