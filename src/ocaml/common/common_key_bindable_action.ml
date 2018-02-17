type t =
    Next_item
  | Prev_item
  | Leave_directory
  | Enter_directory
  | Move_to_another
  | Copy
  | Move
  | Delete
  | Quit
[@@deriving yojson]
