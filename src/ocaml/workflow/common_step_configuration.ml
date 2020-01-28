open Abbrev

type load = unit -> D.Configuration.t Lwt.t
(** Type of step to load configuration *)

type save = D.Configuration.t -> unit Lwt.t
(** Type of step to save the configuration *)
