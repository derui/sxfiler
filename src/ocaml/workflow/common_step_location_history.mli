open Abbrev
open Sxfiler_core

type generate_record = Common_step_common.now -> Path.t -> D.Location_history.Record.t

val generate_record : generate_record
(** [generate_record now location] returns new record *)
