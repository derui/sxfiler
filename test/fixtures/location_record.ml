open Sxfiler_core
module D = Sxfiler_domain

(** make fixture for {!D.Location_record} *)
let fixture ~location ~timestamp = {D.Location_record.location = Path.of_string location; timestamp}

(** utility function to shortcut creation fixture with list *)
let fixtures records = List.map (fun (location, timestamp) -> fixture ~location ~timestamp) records
