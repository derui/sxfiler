(** Workflows for completer *)

include module type of Completer_intf

val initialize : Initialize.input -> (unit, [> `Step_completer_instance of (module C.Instance) S.Context.t ]) S.t
(** The workflow to initialize collection for candidates *)

val complete :
  Complete.input ->
  ( event list,
    [> `Completer_instance      of (module D.Completer.Instance) S.Context.t
    | `Step_completer_instance of (module C.Instance) S.Context.t
    ] )
  S.t
(** The workflow to complete candidates with input from collection initialized before. *)
