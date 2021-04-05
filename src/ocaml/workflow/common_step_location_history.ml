open Abbrev

let generate_record location =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun ctx -> `Step_common_instance ctx) in
  let module I = (val instance : Common_step_common.Instance) in
  S.return @@ D.Location_history.Record.make ~location ~timestamp:(I.now ())
