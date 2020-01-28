open Abbrev

type provide_collection = unit -> D.Completer.collection Lwt.t
(** collection provider *)

type update_collection = D.Completer.collection -> unit Lwt.t
(** collection updater *)

type read = provide_collection -> (module D.Completer.Instance) -> string -> D.Completer.candidates Lwt.t
(** signature to get candidates from collection with input *)

val read : read
(** Implementation for step [read]. Notice that this step caches collection provided dependency function, so need to
    recreate step when collection is updated*)
