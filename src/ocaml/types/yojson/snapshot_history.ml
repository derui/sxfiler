include Sxfiler_types.Snapshot_history

module Js = struct
  type t = {
    records : Snapshot_record.t list;
    max_records: int [@key "maxRecords"]
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson Js.{
      records = t.records;
      max_records = t.max_records;
    }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    records = v.Js.records;
    max_records = v.Js.max_records;
  }
