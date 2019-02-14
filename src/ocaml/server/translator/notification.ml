open Sxfiler_core
module D = Sxfiler_domain.Notification

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
    | Message body -> `Assoc [("type", `String "message"); ("body", `String body)]
    | Progress body ->
      `Assoc
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

type t =
  { id : string
  ; level : Level.t
  ; body : Body.t }
[@@deriving yojson]

let of_domain t =
  let level =
    match t.D.level with Info -> Level.Info | Warning -> Level.Warning | Error -> Level.Error
  in
  {id = Uuidm.to_string t.D.id; body = Body.of_domain t.body; level}

let to_domain t =
  let level =
    match t.level with Info -> D.Level.Info | Warning -> D.Level.Warning | Error -> D.Level.Error
  in
  let body = Body.to_domain t.body in
  D.make ~id:(Uuidm.of_string t.id |> Option.get_exn) ~body ~level
