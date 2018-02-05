module M = Sxfiler_common.Message

type t =
    Next_item
  | Prev_item
  | Leave_directory
  | Enter_directory
  | Quit
[@@deriving yojson]

let to_message = function
  | Next_item -> M.select_next_item 1
  | Prev_item -> M.select_prev_item 1
  | Leave_directory -> M.LEAVE_DIRECTORY
  | Enter_directory -> M.ENTER_DIRECTORY
  | Quit -> M.REQUEST_QUIT_APPLICATION
