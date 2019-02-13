open Sxfiler_rpc.Types.Location_history
module D = Sxfiler_domain.Location_history

let to_yojson t =
  `Assoc
    [ ("records", `List (List.map Location_record.to_yojson t.records))
    ; ("maxRecords", `Int t.max_records) ]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let records = js |> member "records" |> to_list
    and max_records = js |> member "maxRecords" |> to_int in
    let records =
      List.fold_left
        (fun accum record ->
           match accum with
           | Error _ -> accum
           | Ok accum -> (
               match Location_record.of_yojson record with
               | Ok record -> Ok (record :: accum)
               | Error _ as v -> v ) )
        (Ok []) records
    in
    Sxfiler_core.Result.fmap ~f:(fun records -> {records = List.rev records; max_records}) records
  with Type_error (s, _) -> Error s

let of_domain t =
  {records = List.map Location_record.of_domain t.D.records; max_records = t.max_records}
