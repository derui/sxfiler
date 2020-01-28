open Abbrev

type event = F.event
(** type of event *)

type event_handler = event -> unit Lwt.t
(** Event handler signature *)

type publish = event list -> unit Lwt.t
(** Event publisher signature *)

val setup : event_handler list -> publish
(** make publish function via event handlers *)
