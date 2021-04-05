open Sxfiler_core
open Abbrev

val generate_record :
  Path.t ->
  (D.Location_history.Record.t, [> `Step_common_instance of (module Common_step_common.Instance) S.Context.t ]) S.t
(** [generate_record location] returns new record *)
