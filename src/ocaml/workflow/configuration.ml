open Abbrev
include Configuration_intf

let update { Update.key; value } =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_configuration_instance c) in
  let module I = (val instance : Common_step_configuration.Instance) in
  let* store = I.load () |> S.return_lwt in
  let store' = D.Configuration_store.put ~key ~value store in
  let* () = I.save store' |> S.return_lwt in
  S.return [ Updated store' ]
