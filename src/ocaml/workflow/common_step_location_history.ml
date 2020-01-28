open Sxfiler_core
module D = Sxfiler_domain

type generate_record = Common_step_common.now -> Path.t -> D.Location_history.Record.t

let generate_record : generate_record = fun now location -> D.Location_history.Record.make ~location ~timestamp:(now ())
