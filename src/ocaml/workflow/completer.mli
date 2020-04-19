(** Workflows for completer *)

include module type of Completer_intf

val initialize : Common_step_completer.update_collection -> Initialize.work_flow
(** The workflow to initialize collection for candidates *)

val complete :
  Common_step_completer.provide_collection ->
  (module D.Completer.Instance) ->
  Common_step_completer.read ->
  Complete.work_flow
(** The workflow to complete candidates with input from collection initialized before. *)
