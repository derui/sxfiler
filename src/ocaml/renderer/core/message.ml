module Co = Sxfiler_completion.Domain
module T = Sxfiler_rpc.Types

(* messages for completion operation *)
type completion =
  | Setup of string
  | Read of T.Completion.Candidate.t list
  | Tear_down
  | Select_next
  | Select_prev

(* select current action related specified command. *)
type command = Select of string

type t =
  | Update_filer of (Types.File_list_pos.t * T.Filer.t)
  | Update_keymap of T.Key_map.t
  | Update_configuration of T.Configuration.t
  | Move_cursor_to_next
  | Move_cursor_to_prev
  | Toggle_mark
  | Swap_filer
  (* completion handling *)
  | Completion of completion
  | Command of command
  | Change_mode of Types.Mode.t
  | Raise_error of Sxfiler_core.Error.t

type default = Quit
