open Sxfiler_core
(** Item is a domain in a file tree. *)

module Id : Common.Identity.S with type value := string

module Item : sig
  type t = private {
    id : Id.t;
    full_path : Path.t;
    stat : File_stat.t;
  }
  [@@deriving eq, show]

  val full_path : t -> Path.t
  (** getter for full_path *)

  val stat : t -> File_stat.t
  (** getter for stat *)
end

type t = private
  | Marked   of Item.t
  | Unmarked of Item.t
[@@deriving eq, show]

val id : t -> Id.t
(** [id t] unwrap [id] of the [t] *)

val item : t -> Item.t
(** [item t] unwrap item from [t] *)

val make : id:Id.t -> full_path:Path.t -> stat:File_stat.t -> t
(** [make ~full_path ~stat] gets the new [item] with [Unmarked] *)

val is_directory : t -> bool
(** [is_directory v] shortcut function to detect item is a directory or not *)

val is_file : t -> bool
(** [is_file v] shortcut function to detect item is a file or not *)

val is_symlink : t -> bool
(** [is_symlink v] shortcut function to detect item is a symlink or not *)

val mark : t -> t
(** [mark t] returns a new [t] with marked *)

val unmark : t -> t
(** [unmark t] returns a new [t] with unmarked *)

type compare = t -> t -> int

val compare_by : Types.Sort_type.t -> compare
(** [compare_by sort_type] returns compare function by [sort_type] *)
