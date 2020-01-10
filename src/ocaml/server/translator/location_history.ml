module D = Sxfiler_domain.Location_history
module G = Sxfiler_server_generated

let of_domain (t : D.t) =
  {
    G.Filer.LocationHistory.records = List.map Location_record.of_domain t.D.records;
    maxRecordNumber = t.max_record_num;
  }

let to_domain (t : G.Filer.LocationHistory.t) =
  { D.records = List.map Location_record.to_domain t.records; max_record_num = t.maxRecordNumber }
