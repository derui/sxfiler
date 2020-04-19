open Sxfiler_core
(** [File_window] is simple module for window in filer *)

(* types for category of file window *)

type left_side [@@deriving show, eq]

type right_side [@@deriving show, eq]

type free [@@deriving show, eq]

type 'a t = private {
  file_list : File_list.scanned;
  history : Location_history.t;
}
[@@deriving show, eq]

val make_left : file_list:File_list.scanned -> history:Location_history.t -> left_side t
(** make file window for left side *)

val make_right : file_list:File_list.scanned -> history:Location_history.t -> right_side t
(** make file window for right side *)

val as_free : 'a t -> free t
(**[free t] convert free side of [t]. *)

val reload_list : File_list.scanned -> 'a t -> ('a t, [ `Not_same ]) result
(** reload list of [t] without history *)

val move_location : file_list:File_list.scanned -> timestamp:Time.t -> 'a t -> ('a t, [ `Same ]) result
(** move location of [t] with history record *)
