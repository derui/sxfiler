open Abbrev

type load = unit -> D.Configuration_store.t Lwt.t
(** Type of step to load configuration *)

type save = D.Configuration_store.t -> unit Lwt.t
(** Type of step to save the configuration *)
