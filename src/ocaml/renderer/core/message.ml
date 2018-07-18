module T = Sxfiler_types

type t =
  | Update_scanner of T.Scanner.t [@printer fun fmt _ -> Format.fprintf fmt "Update_scanner"]
  | Update_keymap of Key_map.t [@printer fun fmt _ -> Format.fprintf fmt "Update_keymap"]
[@@deriving show]

type default =
  | Quit
