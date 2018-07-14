(** Scanner module provides type to scan file tree. *)

type t = {
  location: string;
  nodes: Node.t list;
  history: Location_history.t;
}

let make ~location ~nodes ~history = {
  location;
  nodes;
  history;
}

let move_location t ~location ~nodes clock =
  let record = Location_record.record_of ~location clock in
  let history = Location_history.add_record t.history ~record in
  make ~location ~nodes ~history
