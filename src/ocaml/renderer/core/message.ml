module T = Sxfiler_types

type t =
  | Update_scanner of T.Scanner.t

type default =
  | Quit
