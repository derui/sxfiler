include Completer_intf

let initialize : Initialize.work_flow =
 fun input ->
  let open S.Infix in
  let* update_collection = S.fetch ~tag:(fun c -> `Step_completer_update_collection c) in
  let ret =
    let collection = input.Initialize.collection in
    let%lwt () = update_collection collection in
    Lwt.return_unit
  in
  S.return ret

let complete : Complete.work_flow =
 fun input ->
  let open S.Infix in
  let* candidates = C.read input.Complete.input in
  let ret =
    let%lwt candidates = candidates in
    Lwt.return [ Completed candidates ]
  in
  S.return ret
