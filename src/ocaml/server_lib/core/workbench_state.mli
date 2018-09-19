(** This module provides backend for workbench. *)

open Workbench_state_abbrev

(** Type of state *)
type t

val empty : t
(** [empty] returns empty state that is immutable. *)

val add : value:D.Workbench.t -> t -> t
(** [add ~value t] returns new instance of [t] that appended [value] to [t]. *)

val remove : id:D.Workbench.id -> t -> t
(** [remove ~id t] returns new instance of [t] that removed the workbench having [id] from [t]. *)

val find : id:D.Workbench.id -> t -> D.Workbench.t option
(** [find ~id t] search the workbench having [id] from state of [t].  *)
