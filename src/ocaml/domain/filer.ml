type left_file_window = File_window.left_side File_window.t [@@deriving eq, show]

type right_file_window = File_window.right_side File_window.t [@@deriving eq, show]

type t = {
  left_file_window : left_file_window;
  right_file_window : right_file_window;
}
[@@deriving show, eq]

let make ~left_file_window ~right_file_window = { left_file_window; right_file_window }

(* converter between file lists *)
let left_to_right : left_file_window -> right_file_window =
 fun File_window.{ file_list; history } -> File_window.make_right ~file_list ~history

let right_to_left : right_file_window -> left_file_window =
 fun File_window.{ file_list; history } -> File_window.make_left ~file_list ~history

let swap_side { left_file_window; right_file_window } =
  let left_file_window = right_to_left right_file_window and right_file_window = left_to_right left_file_window in
  { left_file_window; right_file_window }

let update_left window t =
  { t with left_file_window = File_window.make_left ~file_list:window.File_window.file_list ~history:window.history }

let update_right window t =
  { t with right_file_window = File_window.make_right ~file_list:window.File_window.file_list ~history:window.history }
