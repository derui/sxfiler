open Sxfiler_rpc.Types

class type progress =
  object
    method targeted : float Js.readonly_prop

    method current : float Js.readonly_prop

    method process : Js.js_string Js.t Js.readonly_prop
  end

class type js =
  object
    method id : Js.js_string Js.t Js.readonly_prop

    method _type : Js.js_string Js.t Js.readonly_prop

    method level : int Js.readonly_prop

    method body_message : Js.js_string Js.t Js.readonly_prop

    method body_progress : progress Js.t Js.readonly_prop
  end

let of_js js : Notification.t =
  let open Sxfiler_core.Option in
  let module D = Sxfiler_domain in
  let v =
    D.Notification.Level.of_int js##.level
    >>= fun level ->
    ( match Js.to_string js##._type with
      | "message" -> Some (D.Notification.OneShot {message = Js.to_string js##.body_message})
      | "progress" ->
        Some
          (D.Notification.Progress
             { targeted = js##.body_progress##.targeted
             ; current = js##.body_progress##.current
             ; process = Js.to_string js##.body_progress##.process })
      | _ -> None )
    >>= fun body -> Some {Notification.id = Js.to_string js##.id; level; body}
  in
  get_exn v
