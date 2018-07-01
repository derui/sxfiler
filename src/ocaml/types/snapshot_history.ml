(** {!Snapshot_history} provides management records in history. *)


type t = {
  records: Snapshot_record.t list;
  max_records: int;
}

(** {[make ?max_records ()]} gets new history.
   Use default value of [max_records] is [100] if it did not give any value.
*)
let make ?(max_records=100) () = { records = []; max_records = max 0 max_records}

let sort_by_timestamp = List.sort (fun a b -> Int64.compare
                                      a.Snapshot_record.timestamp
                                      b.Snapshot_record.timestamp)

(** {[add_record t ~record]} makes new record and *)
let add_record t ~record =
  let records = sort_by_timestamp @@ record :: t.records in
  if t.max_records < List.length records then
    {t with records = List.rev records |> List.tl |> List.rev}
  else
    {t with records = records}
