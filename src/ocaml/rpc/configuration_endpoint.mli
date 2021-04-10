(** This module provides endpoint definitions and implementations for Configuration *)

open Abbrev

val get :
  ([> `Step_configuration_instance of (module F.Common_step.Configuration.Instance) S.Context.t ] -> S.Context.value) ->
  Endpoint.t
(** Query endpoint that get current key map *)

val update : 'a F.Configuration.Update.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** Update specified key *)
