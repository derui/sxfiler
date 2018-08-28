open Sxfiler_core
module D = Sxfiler_domain.Location_record
open Sxfiler_rpc.Types.Location_record

let to_yojson t = `Assoc [("location", `String t.location); ("timestamp", `String t.timestamp)]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let location = js |> member "location" |> to_string
    and timestamp = js |> member "timestamp" |> to_string in
    Ok {location; timestamp}
  with Type_error (s, _) -> Error s


let of_domain t =
  {location = Path.to_string t.D.location; timestamp = Int64.to_string t.D.timestamp}
