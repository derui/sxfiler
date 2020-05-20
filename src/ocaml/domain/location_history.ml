open Sxfiler_core

module Record = struct
  type t = {
    location : Path.t;
    timestamp : Time.t;
  }
  [@@deriving eq, show]

  let make ~location ~timestamp = { location; timestamp }
end

(** {!Snapshot_history} provides management records in history. *)
module Location_set = Set.Make (struct
  type t = Record.t

  let compare v1 v2 =
    let v1 = Path.to_string v1.Record.location and v2 = Path.to_string v2.Record.location in
    Stdlib.compare v1 v2
end)

type t = { records : Record.t list } [@@deriving eq, show]

let sort_by_timestamp = List.sort (fun a b -> Time.compare a.Record.timestamp b.timestamp) %> List.rev

(** [make ?max_record_num ()] gets new history.

    Use default value of [max_record_num] is [100] if it did not give any value. *)
let make ?(records = []) () =
  let records = sort_by_timestamp records in
  { records }

(** [add_record t ~record] makes new record and *)
let add_record record t =
  let set = Location_set.of_list (record :: t.records) in
  let records = Location_set.to_seq set |> List.of_seq |> sort_by_timestamp in
  { records }
