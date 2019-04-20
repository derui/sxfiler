(** Filer module provides type to scan file tree. *)

type id = string [@@deriving show]

type t = private
  { id : id
  ; file_tree : File_tree.t
  ; history : Location_history.t
  ; selected_nodes : Node.id list
  ; sort_order : Types.Sort_type.t }
[@@deriving eq, show, make]

(* sort nodes with sort_order in [t] *)

val has_same_id : t -> t -> bool
(** [has_same_id t1 t2] return having same id between [t1] and [t2]. *)

val move_location : t -> file_tree:File_tree.t -> (module Location_record.Clock) -> t
(** [move_location t ~file_tree  (module Clock)] returns updated [t] with
    moved to new location that [file_tree] has.
*)

val update_tree : t -> file_tree:File_tree.t -> t
(** [update_tree t ~file_tree] get new filer is based on [t] and updated file tree from parameter. *)

val find_node : t -> id:id -> Node.t option
(** [find_node t ~id] search node having [id] in filer [t] *)

val select_nodes : t -> ids:Node.id list -> t
(** [select_nodes t ~ids] return new filer that selected nodes specified from [ids]. *)

val deselect_nodes : t -> ids:Node.id list -> t
(** [deselect_nodes t ~ids] return new filer that deselected nodes specified from [ids]. *)

val node_subset : t -> ids:Node.id list -> Node.t list * Node.id list
(** [node_subset t ~ids] returns subset of nodes of [t] that are found in [t] and ids that are not found in [t] *)

(** Signature for repository of scanner. *)
module type Repository = sig
  val resolve : id -> t option Lwt.t
  (** [resolve id] returns scanner instance if already exists. *)

  val store : t -> unit Lwt.t
  (** [store filer] stores [t] to any place. *)
end

(** Factory interface *)
module Factory : sig
  module type S = sig
    val create :
         name:string
      -> file_tree:File_tree.t
      -> history:Location_history.t
      -> sort_order:Types.Sort_type.t
      -> t
    (** [create ~name ~file_tree ~history ~sort_order] gets new instance of filer. *)
  end

  module Make : S
end
