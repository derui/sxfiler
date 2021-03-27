open Abbrev
include Configuration_intf

let update :
    Update.input ->
    ( event list Lwt.t,
      [> `Step_configuration_load of Common_step_configuration.load S.Context.t
      | `Step_configuration_save of Common_step_configuration.save S.Context.t
      ] )
    S.t =
 fun { key; value } ->
  let open S.Infix in
  let* load = S.fetch ~tag:(fun c -> `Step_configuration_load c) in
  let* save = S.fetch ~tag:(fun c -> `Step_configuration_save c) in
  let ret =
    let%lwt store = load () in
    let store' = D.Configuration_store.put ~key ~value store in
    let%lwt () = save store' in
    Lwt.return [ Updated store' ]
  in
  S.return ret
