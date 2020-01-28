open Sxfiler_core
open Abbrev

type resolve_keymap = unit -> D.Keymap.t Lwt.t
(** step to resolve key map *)

type store_keymap = D.Keymap.t -> unit Lwt.t
(** step to store key map *)

type load_error =
  [ `Not_found
  | `Illegal_keymap of string
  ]
[@@deriving eq, show]

type load_keymap = Path.t -> (D.Keymap.t, load_error) result Lwt.t
(** step to load keymap from the place *)
