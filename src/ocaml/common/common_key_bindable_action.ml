type t =
    Next_item
  | Prev_item
  | Leave_directory
  | Enter_directory
  | Change_active_pane
  | Copy
  | Move
  | Delete
  | Rename
  | Quit
[@@deriving yojson]
