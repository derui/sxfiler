(** Workflows for completer *)

include module type of Completer_intf

val initialize : Initialize.work_flow
(** The workflow to initialize collection for candidates *)

val complete : Complete.work_flow
(** The workflow to complete candidates with input from collection initialized before. *)
