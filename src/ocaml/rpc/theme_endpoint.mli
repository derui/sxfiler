(** This module provides endpoint definitions and implementations for Theme *)

open Abbrev

val get :
  ([> `Step_theme_instance of (module F.Common_step.Theme.Instance) S.Context.t ] -> S.Context.value) -> Endpoint.t
(** endpoint to get theme *)

val update :
  ([> `Step_theme_instance of (module F.Common_step.Theme.Instance) S.Context.t ] -> S.Context.value) -> Endpoint.t
(** Update theme configuration *)
