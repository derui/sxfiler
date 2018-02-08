module M = Sxfiler_common.Message

type t =
    Next_item
  | Prev_item
  | Leave_directory
  | Enter_directory
  | Move_to_another
  | Quit
[@@deriving yojson]

let to_message = function
  | Next_item -> M.select_next_item 1
  | Prev_item -> M.select_prev_item 1
  | Leave_directory -> M.leave_directory
  | Enter_directory -> M.enter_directory
  | Quit -> M.request_quit_application
  | Move_to_another -> M.move_to_another
