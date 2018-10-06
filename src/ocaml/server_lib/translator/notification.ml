module D = Sxfiler_domain.Notification
open Sxfiler_rpc.Types.Notification

let to_yojson t =
  let common = [("id", `String t.id); ("level", `Int (D.Level.to_int t.level))] in
  let assoc =
    match t.body with
    | D.OneShot body -> [("type", `String "message"); ("body", `String body.message)] @ common
    | D.Progress body ->
      [ ("type", `String "progress")
      ; ( "body"
        , `Assoc
            [ ("targeted", `Float body.targeted)
            ; ("current", `Float body.current)
            ; ("process", `String body.process) ] ) ]
      @ common
  in
  `Assoc assoc

let of_domain t = {id = Uuidm.to_string t.D.id; body = t.body; level = t.level}
