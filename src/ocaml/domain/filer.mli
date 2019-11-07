(** Filer module provides type to scan file tree. *)

type id = Uuidm.t [@@deriving show]

module Marked_item_set : sig
  include Set.S with type elt = File_item.id

  val pp : Format.formatter -> t -> unit
end

type t = private {
  id : id;
  name : string;
  file_list : File_list.t;
  history : Location_history.t;
  marked_items : Marked_item_set.t;
  sort_order : Types.Sort_type.t;
}
[@@deriving eq, show, make]

(* sort items with sort_order in [t] *)

val has_same_id : t -> t -> bool
(** [has_same_id t1 t2] return having same id between [t1] and [t2]. *)

val move_location : t -> file_list:File_list.t -> (module Location_record.Clock) -> t
(** [move_location t ~file_tree (module Clock)] returns updated [t] with moved to new location that
    [file_tree] has. *)

val update_list : t -> file_list:File_list.t -> t
(** [update_tree t ~file_tree] get new filer is based on [t] and updated file tree from parameter. *)

val find_item : t -> id:File_item.id -> File_item.t option
(** [find_item t ~id] search item having [id] in filer [t] *)

val add_mark : t -> ids:File_item.id list -> t
(** [add_mark t ~ids] return new filer that selected items specified from [ids]. *)

val remove_mark : t -> ids:File_item.id list -> t
(** [remove_mark t ~ids] return new filer that deselected items specified from [ids]. *)

(** Signature for repository of scanner. *)
module type Repository = Filer_intf.Repository with type t := t and type id := id

(** Factory interface *)
module Factory : sig
  module type S = Filer_intf.Factory with type t := t

  module Make (G : Id_generator_intf.Gen_random with type id = id) : S
end
