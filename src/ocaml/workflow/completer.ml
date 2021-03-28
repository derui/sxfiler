include Completer_intf

let initialize input =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_completer_instance c) in
  let module I = (val instance : C.Instance) in
  let collection = input.Initialize.collection in
  I.update_collection collection |> S.return_lwt

let complete input =
  let open S.Infix in
  let* candidates = C.read input.Complete.input in
  S.return [ Completed candidates ]
