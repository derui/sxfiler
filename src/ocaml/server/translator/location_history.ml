module D = Sxfiler_domain.Location_history

type t =
  { records : Location_record.t list
  ; max_record_num : int }
[@@deriving yojson]

let of_domain t =
  {records = List.map Location_record.of_domain t.D.records; max_record_num = t.max_record_num}

let to_domain t =
  {D.records = List.map Location_record.to_domain t.records; max_record_num = t.max_record_num}
