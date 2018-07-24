module T = Sxfiler_types

(* messages for completion operation *)
type completion =
  | Setup of T.Completion.Source_class.t
  | Read of T.Completion.result
  | Tear_down

type t =
  | Update_scanner of T.Scanner.t
  | Update_keymap of Key_map.t
  | Update_configuration of T.Configuration.t
  (* switch mode *)
  | Switch_mode of Types.Mode.t
  | Move_cursor_to_next
  | Move_cursor_to_prev
  (* completion handling *)
  | Completion of completion

type default =
  | Quit
