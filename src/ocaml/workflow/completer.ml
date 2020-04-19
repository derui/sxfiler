include Completer_intf

let initialize : Common_step_completer.update_collection -> Initialize.work_flow =
 fun update_collection input ->
  let collection = input.Initialize.collection in
  let%lwt () = update_collection collection in
  Lwt.return_unit

let complete :
    Common_step_completer.provide_collection ->
    (module D.Completer.Instance) ->
    Common_step_completer.read ->
    Complete.work_flow =
 fun provichde_collection instance read input ->
  let%lwt candidates = read provichde_collection instance input.Complete.input in
  Lwt.return [ Completed candidates ]
