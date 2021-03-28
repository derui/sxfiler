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

module Add_key_binding = struct
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
    action : D.Common.Not_empty_string.t;
  }
end

module Remove_key_binding = struct
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
  }
end

module Reload = struct
  type input = { path : Path.t }
end

type commands =
  | Add_key_binding    of Add_key_binding.input
  | Remove_key_binding of Remove_key_binding.input
  | Reload             of Reload.input
