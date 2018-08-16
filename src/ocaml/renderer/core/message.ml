module D = Sxfiler_domain
module Co = Sxfiler_completion.Domain

(* messages for completion operation *)
type completion =
  | Setup of string
  | Read of Co.result
  | Tear_down
  | Select_next
  | Select_prev

type command =
  (* select current action related specified command. *)
  | Select of string

type t =
  | Update_scanner of D.Scanner.t
  | Update_keymap of string D.Key_map.t
  | Update_configuration of D.Configuration.t
  (* switch mode *)
  | Switch_mode of Types.Mode.t
  | Move_cursor_to_next
  | Move_cursor_to_prev
  | Swap_scanner
  (* completion handling *)
  | Completion of completion
  | Command of command

type default =
  | Quit
