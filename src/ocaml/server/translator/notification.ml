open Sxfiler_core
module D = Sxfiler_domain.Notification

(** level of notification. *)
module Level = struct
  type t =
    | Info [@name "info"]
    | Warning [@name "warning"]
    | Error [@name "error"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain = function D.Level.Info -> Info | Warning -> Warning | Error -> Error

  let to_domain = function
    | Info -> D.Level.Info
    | Warning -> D.Level.Warning
    | Error -> D.Level.Error
end

(** body describes body of the notification. *)
module Body = struct
  type t =
    | Message of string
    | Progress of {process : string; current : float; targeted : float}
  [@@deriving show]

  let of_domain = function
    | D.Message message -> Message message
    | Progress v -> Progress {process = v.process; current = v.current; targeted = v.targeted}

  let to_domain = function
    | Message message -> D.Message message
    | Progress v -> Progress {process = v.process; current = v.current; targeted = v.targeted}

  let to_json = function
    | Message body -> `Assoc [("type", `String "message"); ("body", `String body)]
    | Progress body ->
        `Assoc
          [ ("type", `String "progress")
          ; ( "body"
            , `Assoc
                [ ("targeted", `Float body.targeted)
                ; ("current", `Float body.current)
                ; ("process", `String body.process) ] ) ]

  let of_json js =
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
      | _ -> Error (Protocol_conv_json.Json.make_error @@ Printf.sprintf "Unknown type: %s" typ)
    with Type_error (s, value) -> Error (Protocol_conv_json.Json.make_error ~value s)

  let of_json_exn js =
    match of_json js with Ok v -> v | Error e -> raise (Protocol_conv_json.Json.Protocol_error e)
end

type t =
  { id : string
  ; level : Level.t
  ; body : Body.t }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  let level = Level.of_domain t.D.level in
  {id = Uuidm.to_string t.D.id; body = Body.of_domain t.body; level}

let to_domain t =
  let level = Level.to_domain t.level in
  let body = Body.to_domain t.body in
  D.make ~id:(Uuidm.of_string t.id |> Option.get_exn) ~body ~level
