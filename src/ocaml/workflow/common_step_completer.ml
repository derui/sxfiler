open Abbrev
include Common_step_intf.Completer

let read input =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_completer_instance c) in
  let* completer = S.fetch ~tag:(fun c -> `Completer_instance c) in
  let module I = (val completer : D.Completer.Instance) in
  let module Instance = (val instance : Instance) in
  let* collection = Instance.provide_collection () |> S.return_lwt in
  I.Completer.read ~input ~collection I.this |> S.return
