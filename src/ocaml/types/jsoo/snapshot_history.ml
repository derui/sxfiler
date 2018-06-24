include Sxfiler_types.Snapshot_history

class type js = object
  method records: Snapshot_record.js Js.t Js.js_array Js.t Js.readonly_prop
  method maxRecords: int Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    records = Js.array_map Snapshot_record.of_js js##.records |> Js.to_array |> Array.to_list;
    max_records = js##.maxRecords;
  }
