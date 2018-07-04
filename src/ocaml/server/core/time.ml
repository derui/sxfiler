(** {!Time} module provides utility functions for time in sxfiler *)

(** [time_to_int64 time] convert [time] as unixtime to int64 with microsecond resolution. *)
let time_to_int64 time =
  let unixtime = float_of_int @@ int_of_float time in
  let utc, _ = Unix.mktime @@ Unix.gmtime time
  and milliseconds = int_of_float ((time -. unixtime) *. 1000.0) in
  let utc = Int64.(mul (of_float utc) 1000L) in
  Int64.(add (of_int milliseconds) utc)
