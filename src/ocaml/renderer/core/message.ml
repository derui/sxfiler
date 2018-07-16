module T = Sxfiler_types

type t =
  | Update_scanner of T.Scanner.t [@printer fun fmt _ -> Format.fprintf fmt "Update_scanner"]
[@@deriving show]

type default =
  | Quit
