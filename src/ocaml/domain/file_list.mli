open Sxfiler_core

(** Filer module provides type to scan file tree. *)

module Id : Common.Identity.S with type value := string

(** The type of {!File_list} scanned before. *)
type scanned = private
  | Valid       of {
      id : Id.t;
      location : Path.t;
      items : File_item.t list;
      file_item_order : File_item_order.t;
    }
  | No_location of {
      id : Id.t;
      location : Path.t;
      file_item_order : File_item_order.t;
    }
[@@deriving eq, show]

type unscanned = private {
  id : Id.t;
  location : Path.t;
  file_item_order : File_item_order.t;
}
[@@deriving eq, show]
(** The type of {!File_list} that did not scanned. *)

val location : scanned -> Path.t
(** [location t] is getter for [location] in [t] *)

val make : id:Id.t -> location:Path.t -> sort_type:Types.Sort_type.t -> unscanned
(** [make ~id ~location ~sort_order] makes the instance [t] from arguments *)

val reload : [ `Scanned     of File_item.t list | `No_location ] -> scanned -> scanned
(** [reload result scanned] update state of [scanned] with result. When [result] is [`No_location], result type is
    [No_location] *)

val scan : [ `Scanned     of File_item.t list | `No_location ] -> unscanned -> scanned
(** [scan result scanned] reflect result to [scanned]. When [result] is [`No_location], result type is [No_location] *)

val change_location : location:Path.t -> scanned -> unscanned
(** [change_location ~location t] changes the location of [t] to [location] *)

val mark_items : ids:File_item.Id.t list -> scanned -> scanned
(** [mark_items ~ids t] return new filer that selected items specified from [ids]. *)

val unmark_items : ids:File_item.Id.t list -> scanned -> scanned
(** [unmark_items ~ids t] return new filer that deselected items specified from [ids]. *)

val marked_items : scanned -> File_item.t list
(** [marked_items t] returns collection of item that are marked *)

val items : scanned -> File_item.t list
(** [items t] returns items in [t] *)

val find_item : id:File_item.Id.t -> scanned -> File_item.t option
(** [find_item ~id t] find item that have [id] from [t] *)

type file_diff =
  [ `In_left  of File_item.t
  | `In_right of File_item.t
  ]
[@@deriving show, eq]

val diff : left:scanned -> right:scanned -> file_diff list
(** [diff ~left ~right] return difference between [left] and [right] file lists. *)
