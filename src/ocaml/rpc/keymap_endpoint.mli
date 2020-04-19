(** This module provides endpoint definitions and implementations for Keymap *)

open Sxfiler_core
open Abbrev

type add_key_binding = F.Keymap.Add_key_binding.work_flow -> Endpoint.t

val add_key_binding : add_key_binding
(** endpoint to add key binding to current key map *)

type remove_key_binding = F.Keymap.Remove_key_binding.work_flow -> Endpoint.t

val remove_key_binding : remove_key_binding
(** endpoint to remove key binding from current key map *)

type reload = Path.t -> F.Keymap.Reload.work_flow -> Endpoint.t

val reload : reload
(** Reload key map from specified path *)

(* query endpoints *)
type get = F.Common_step.Keymap.resolve_keymap -> Endpoint.t

val get : get
(** Query endpoint that get current key map *)
