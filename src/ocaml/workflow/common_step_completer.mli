open Abbrev
include module type of Common_step_intf.Completer
(** signature to get candidates from collection with input *)

val read : string ->
    ( D.Completer.candidates,
      [> `Step_completer_instance of (module Instance) S.Context.t
      | `Completer_instance      of (module D.Completer.Instance) S.Context.t
      ] )
    S.t
  (** signature to get candidates from collection with input *)
(** Implementation for step [read]. Notice that this step caches collection provided dependency function, so need to
    recreate step when collection is updated*)
