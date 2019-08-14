open Sxfiler_core
open Fun

(** {!Snapshot_history} provides management records in history. *)
module Location_set = Set.Make (struct
  type t = Location_record.t

  let compare v1 v2 =
    let v1 = Path.to_string v1.Location_record.location
    and v2 = Path.to_string v2.Location_record.location in
    Stdlib.compare v1 v2
end)

type t =
  { records : Location_record.t list
  ; max_record_num : int }
[@@deriving eq, show]

(** {[ make ?max_record_num () ]} gets new history. Use default value of [max_record_num] is [100]
    if it did not give any value. *)
let make ?(max_record_num = 100) () = {records = []; max_record_num = max 0 max_record_num}

let sort_by_timestamp =
  List.sort (fun a b -> Int64.compare a.Location_record.timestamp b.Location_record.timestamp)
  %> List.rev

(** {[ add_record t ~record ]} makes new record and *)
let add_record t ~record =
  let set = Location_set.of_list (record :: t.records) in
  let records = Location_set.to_seq set |> List.of_seq |> sort_by_timestamp in
  if t.max_record_num < List.length records then
    {t with records = List.rev records |> List.tl |> List.rev}
  else {t with records}
