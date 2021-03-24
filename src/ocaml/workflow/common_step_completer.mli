include module type of Common_step_intf.Completer
(** signature to get candidates from collection with input *)

val read : read
(** Implementation for step [read]. Notice that this step caches collection provided dependency function, so need to
    recreate step when collection is updated*)
