(** This module defines translator for {Location_history} module to translate from domain to
    outer model.
*)
module D = Sxfiler_domain.Location_history

type t = {
  records : Location_record.t list;
  max_records: int [@key "maxRecords"]
} [@@deriving yojson]

let of_domain t = {
  records = List.map Location_record.of_domain t.D.records;
  max_records = t.max_records;
}

let to_domain t = {
  D.records = List.map Location_record.to_domain t.records;
  max_records = t.max_records;
}
