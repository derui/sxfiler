type left_file_window = File_window.left_side File_window.t [@@deriving eq, show]

type right_file_window = File_window.right_side File_window.t [@@deriving eq, show]

type t = private {
  left_file_window : left_file_window;
  right_file_window : right_file_window;
}
[@@deriving show, eq]
(** Type of filer *)

val make : left_file_window:left_file_window -> right_file_window:right_file_window -> t
(** [make ~left_file_window ~right_file_window] makes new instance of [t] *)

val swap_side : t -> t
(** [swap_side t] swap file list between left and right *)

val update_left : File_window.free File_window.t -> t -> t
(** [update_left window t] update left side file window *)

val update_right : File_window.free File_window.t -> t -> t
(** [update_right window t] update right side file window *)
