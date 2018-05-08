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
  | Jump
  | Quit
  | Toggle_mark
  | Make_dir
  | History
  | Change_permission
  | Toggle_bookmark

let of_string = function
  | "Next_item" -> Next_item
  | "Prev_item" -> Prev_item
  | "Leave_directory" -> Leave_directory
  | "Enter_directory" -> Enter_directory
  | "Change_active_pane" -> Change_active_pane
  | "Copy" -> Copy
  | "Move" -> Move
  | "Delete" -> Delete
  | "Rename" -> Rename
  | "Jump" -> Jump
  | "Quit" -> Quit
  | "Toggle_mark" -> Toggle_mark
  | "Make_dir" -> Make_dir
  | "History" -> History
  | "Change_permission" -> Change_permission
  | "Toggle_bookmark" -> Toggle_bookmark
  | _ -> failwith "Unknown action"
