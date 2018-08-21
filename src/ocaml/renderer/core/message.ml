module Co = Sxfiler_completion.Domain
module T = Sxfiler_rpc.Types

(* messages for completion operation *)
type completion =
  | Setup of string
  | Read of T.Completion.Candidate.t list
  | Tear_down
  | Select_next
  | Select_prev

type command =
  (* select current action related specified command. *)
  | Select of string

type t =
  | Update_scanner of T.Scanner.t
  | Update_keymap of T.Key_map.t
  | Update_configuration of T.Configuration.t
  | Move_cursor_to_next
  | Move_cursor_to_prev
  | Swap_scanner
  (* completion handling *)
  | Completion of completion
  | Command of command

type default =
  | Quit
