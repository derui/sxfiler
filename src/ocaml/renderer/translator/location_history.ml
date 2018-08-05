open Sxfiler_core
open Sxfiler_domain.Location_history

class type js = object
  method records: Location_record.js Js.t Js.js_array Js.t Js.readonly_prop
  method maxRecords: int Js.readonly_prop
end

let of_js ?(system=(module System.Real : System.S)) js : t =
  {
    records = Js.array_map (Location_record.of_js ~system) js##.records |> Js.to_array |> Array.to_list;
    max_records = js##.maxRecords;
  }

let to_js t : js Js.t = object%js
  val records = List.map Location_record.to_js t.records
                |> Array.of_list
                |> Js.array
  val maxRecords = t.max_records
end
