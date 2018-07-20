module T = Sxfiler_types

type t =
  | Update_scanner of T.Scanner.t
  | Update_keymap of Key_map.t
  | Update_configuration of T.Configuration.t
  (* switch mode *)
  | Switch_mode of Types.Mode.t

type default =
  | Quit
