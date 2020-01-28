open Sxfiler_workflow
open Sxfiler_domain

val resolve_keymap : (module Statable.S with type state = Keymap.t) -> Common_step.Keymap.resolve_keymap
(** implementation for [Common_step.Keymap.resolve_keymap] *)

val store_keymap : (module Statable.S with type state = Keymap.t) -> Common_step.Keymap.store_keymap
(** implementation for [Common_step.Keymap.store_keymap] *)

val load_keymap : Common_step.Keymap.load_keymap
(** implementation for [Common_step.Keymap.load_keymap] *)
