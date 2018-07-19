module T = Sxfiler_types

type t =
  | Update_scanner of T.Scanner.t
  | Update_keymap of Key_map.t
  | Update_configuration of T.Configuration.t
  (* Enter and leave from file tree context *)
  | Enter_file_tree_context
  | Leave_file_tree_context

type default =
  | Quit
