open Sxfiler_core
module D = Sxfiler_domain
module L = D.Location_history
module G = Sxfiler_generated

type error =
  | Invalid_path       of string
  | Invalid_timestamp  of string
  | Invalid_record_num of int
[@@deriving eq, show]

module Record = struct
  module G = Sxfiler_generated

  let of_domain (t : L.Record.t) =
    { G.Filer.LocationRecord.location = Path.to_string t.location; timestamp = Time.to_rfc3339 t.timestamp }

  let to_domain (t : G.Filer.LocationRecord.t) =
    let open Result.Infix in
    let* timestamp = Time.of_rfc3339 t.timestamp |> Option.to_result ~none:(Invalid_timestamp t.timestamp) in
    Path.of_string t.location
    |> Result.map (fun location -> L.Record.make ~location ~timestamp)
    |> Result.map_error (fun _ -> Invalid_path t.location)
end

let of_domain (t : L.t) =
  {
    G.Filer.LocationHistory.records = List.map Record.of_domain t.records;
    max_record_number = D.Common.Positive_number.value t.max_record_num;
  }

let to_domain (t : G.Filer.LocationHistory.t) =
  let open Result.Infix in
  let* max_record_num =
    D.Common.Positive_number.make t.max_record_number |> Option.to_result ~none:(Invalid_record_num t.max_record_number)
  in
  let* records =
    List.fold_left
      (fun list v ->
        let* list = list in
        let* record = Record.to_domain v in
        Ok (record :: list))
      (Ok []) t.records
  in
  let records = List.rev records in
  L.make ~records ~max_record_num () |> Result.ok
