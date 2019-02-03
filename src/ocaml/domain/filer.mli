(** Filer module provides type to scan file tree. *)

open Sxfiler_core

type id = string [@@deriving show]

type t =
  { id : id
  ; location : Path.t
  ; nodes : Node.t list
  ; history : Location_history.t
  ; selected_nodes : Node.id list
  ; sort_order : Types.Sort_type.t }
[@@deriving show]

(* sort nodes with sort_order in [t] *)

val move_location :
  t -> location:Path.t -> nodes:Node.t list -> (module Location_record.Clock) -> t
(** [move_location t ~location ~nodes (module Clock)] returns updated [t] with
    moved to [location] with [nodes].
*)

val make :
  id:id
  -> location:Path.t
  -> nodes:Node.t list
  -> history:Location_history.t
  -> sort_order:Types.Sort_type.t
  -> t
(** [make ~id ~location ~nodes ~history ~sort_order] gets new instance of filer. *)

val update_nodes : t -> nodes:Node.t list -> t
(** [update_nodes t ~nodes] get new filer is based on [t] and updated nodes from parameter. *)

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
  val resolve : string -> t option Lwt.t
  (** [resolve id] returns scanner instance if already exists. *)

  val store : t -> unit Lwt.t
  (** [store filer] stores [t] to any place. *)
end
