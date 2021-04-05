(** This module provides endpoint definitions and implementations for Keymap *)

open Sxfiler_core
open Abbrev

val add_key_binding :
  ([> `Step_keymap_instance of (module F.Common_step.Keymap.Instance) S.Context.t ] -> S.Context.value) -> Endpoint.t

(** endpoint to add key binding to current key map *)

val remove_key_binding :
  ([> `Step_keymap_instance of (module F.Common_step.Keymap.Instance) S.Context.t ] -> S.Context.value) -> Endpoint.t
(** endpoint to remove key binding from current key map *)

val reload :
  Path.t ->
  ([> `Step_keymap_instance of (module F.Common_step.Keymap.Instance) S.Context.t ] -> S.Context.value) ->
  Endpoint.t
(** Reload key map from specified path *)

(* query endpoints *)

val get :
  ([> `Step_keymap_instance of (module F.Common_step.Keymap.Instance) S.Context.t ] -> S.Context.value) -> Endpoint.t
(** Query endpoint that get current key map *)
