open Sxfiler_core

type store_configuration = Sxfiler_domain.Theme.Configuration.t -> unit Lwt.t

val list_theme : Path.t -> Sxfiler_domain.Theme.Definition.t list Lwt.t
(** [list_theme directory] load all theme definitions in [directory] *)

val store_theme : store_configuration -> Path.t -> Sxfiler_workflow.Common_step.Theme.Store_theme.t
(** [store_theme directory] returns the closure to store theme. *)
