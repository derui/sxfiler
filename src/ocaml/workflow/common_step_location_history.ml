open Abbrev
include Common_step_intf.Location_history

let generate_record location =
  let open S.Infix in
  let* now = S.fetch ~tag:(fun ctx -> `Step_common_now ctx) in
  S.return @@ D.Location_history.Record.make ~location ~timestamp:(now ())
