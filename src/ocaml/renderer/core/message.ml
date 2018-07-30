module T = Sxfiler_types

(* messages for completion operation *)
type completion =
  | Setup of T.Completion.Source_class.t * string
  | Read of T.Completion.result
  | Tear_down

type command =
  (* select current action related specified command. *)
  | Select of Callable_action.t

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
  | Command of command

type default =
  | Quit
