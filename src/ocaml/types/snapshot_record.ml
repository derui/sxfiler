(** [Snapshot_record] provides types for record of history  *)
type t = {
  directory: string;
  timestamp: int64;
}

let make ~directory ~timestamp = {directory; timestamp}
