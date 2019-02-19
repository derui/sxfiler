(** {!Snapshot_history} provides management records in history. *)

type t =
  { records : Location_record.t list
  ; max_record_num : int }
[@@deriving eq, show]

(** {[make ?max_record_num ()]} gets new history.
   Use default value of [max_record_num] is [100] if it did not give any value.
*)
let make ?(max_record_num = 100) () = {records = []; max_record_num = max 0 max_record_num}

let sort_by_timestamp =
  List.sort (fun a b -> Int64.compare a.Location_record.timestamp b.Location_record.timestamp)

(** {[add_record t ~record]} makes new record and *)
let add_record t ~record =
  let records = sort_by_timestamp @@ (record :: t.records) in
  if t.max_record_num < List.length records then
    {t with records = List.rev records |> List.tl |> List.rev}
  else {t with records}
