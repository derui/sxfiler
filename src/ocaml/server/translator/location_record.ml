open Sxfiler_core
module D = Sxfiler_domain.Location_record
module G = Sxfiler_server_generated

let of_domain (t : D.t) =
  {
    G.Filer.LocationRecord.location = Path.to_string t.D.location;
    timestamp = Int64.to_string t.D.timestamp;
  }

let to_domain (t : G.Filer.LocationRecord.t) =
  { D.location = Path.of_string t.location; timestamp = Int64.of_string t.timestamp }
