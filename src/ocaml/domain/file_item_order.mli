(** This module provides order of items in file list. *)

type id = File_item.Id.t

type sort_level = Types.sort_level

type sort_type = Types.Sort_type.t

module Order_mapping : sig
  type t
end

type t = private {
  sort_type : sort_type;
  order_mapping : Order_mapping.t;
}
[@@deriving eq, show]

val make : sort_type:sort_type -> t
(** [make ~sort_type] make new mapping. *)

val update_order_mapping : ?sort_type:sort_type -> file_items:File_item.t list -> t -> t
(** [update_order_mapping ?sort_type ~items t] get new instance of [t] with new order mapping computed from given
    [items]. Use [sort_type] to compute order mapping if given. *)

val to_alist : t -> (id * sort_level) list
(** [to_alist t] convert to associate list *)
