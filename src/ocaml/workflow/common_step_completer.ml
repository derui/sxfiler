open Abbrev
include Common_step_intf.Completer

let read : read =
 fun input ->
  let open S.Infix in
  let* provide_collection = S.fetch ~tag:(fun c -> `Step_completer_provide_collection c) in
  let* completer = S.fetch ~tag:(fun c -> `Completer_instance c) in
  let module I = (val completer : D.Completer.Instance) in
  let result =
    let%lwt collection = provide_collection () in
    I.Completer.read ~input ~collection I.this |> Lwt.return
  in
  S.return result
