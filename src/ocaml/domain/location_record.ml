(** [Snapshot_record] provides types for record of history  *)

open Sxfiler_core
type t = {
  location: Path.t;
  timestamp: int64;
}

(** Signature to get current clock to make record with current time. *)
module type Clock = sig
  (** Get unix time that should have millisecond resolution. So if 1 second of unix time given,
      this function should return [1000L] as unix time.
  *)
  val unixtime : unit -> int64
end

let record_of ~location clock =
  let module C = (val clock: Clock) in
  let now = C.unixtime () in
  {
    location; timestamp = now;
  }
