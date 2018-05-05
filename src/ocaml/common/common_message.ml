
type t =
  | Close_dialog
  | Close_dialog_with_action of t
  | Refresh_candidates_request of string
  | Change_permission of int
  | Jump_directory of string
[@@derivng variants]
