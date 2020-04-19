open Abbrev
open Sxfiler_core

type resolve_keymap = unit -> D.Keymap.t Lwt.t

type store_keymap = D.Keymap.t -> unit Lwt.t

type load_error =
  [ `Not_found
  | `Illegal_keymap of string
  ]
[@@deriving eq, show]

type load_keymap = Path.t -> (D.Keymap.t, load_error) result Lwt.t
