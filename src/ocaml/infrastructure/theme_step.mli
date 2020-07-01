open Sxfiler_core

val store_theme : Path.t -> Sxfiler_workflow.Common_step.Theme.Store_theme.t
(** [store_theme directory] returns the closure to store theme. *)

val remove_theme : Path.t -> Sxfiler_workflow.Common_step.Theme.Remove_theme.t
(** [remove_theme directory] returns the closure to remove theme. *)

val list_theme : Path.t -> Sxfiler_workflow.Common_step.Theme.List_theme.t
(** [remove_theme theme_directory] returns the closure to list theme from [theme_directory] *)
