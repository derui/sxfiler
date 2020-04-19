open Abbrev

type event = F.event

type event_handler = F.event -> unit Lwt.t
(** Type signature of handler for events raised from Filer workflows *)

type publish = F.event list -> unit Lwt.t
(** Type signature of publisher to publish events *)

let setup handlers : publish = fun events -> Lwt_list.iter_p (fun handler -> Lwt_list.iter_p handler events) handlers
