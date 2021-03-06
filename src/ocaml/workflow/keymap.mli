open Abbrev
open Sxfiler_core

type error =
  | Empty_context
  | Invalid_key    of string
  | Invalid_keymap of Common_step_keymap.load_error
[@@deriving eq, show]

type event =
  | Added    of D.Keymap.t
  | Removed  of D.Keymap.t
  | Reloaded of D.Keymap.t
[@@deriving eq, show]

module Add_key_binding : sig
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
    action : D.Common.Not_empty_string.t;
  }

  type work_flow = input -> (event list, error) result Lwt.t
  (** workflow to add a key binding for action to key map *)
end

module Remove_key_binding : sig
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
  }

  type work_flow = input -> (event list, error) result Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

module Reload : sig
  type input = { path : Path.t }

  type work_flow = input -> (event list, error) result Lwt.t
  (** workflow to reload key map from path *)
end

type commands =
  | Add_key_binding    of Add_key_binding.input
  | Remove_key_binding of Remove_key_binding.input

val add_key_binding : Common_step_keymap.resolve_keymap -> Common_step_keymap.store_keymap -> Add_key_binding.work_flow
(** implementation to add binding to key map *)

val remove_key_binding :
  Common_step_keymap.resolve_keymap -> Common_step_keymap.store_keymap -> Remove_key_binding.work_flow
(** implementation to remove binding from key map *)

val reload : Common_step_keymap.load_keymap -> Common_step_keymap.store_keymap -> Reload.work_flow
(** implementation of workflow to reload whole key map *)
