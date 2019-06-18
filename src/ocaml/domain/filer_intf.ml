(** Signature for repository of scanner. *)
module type Repository = sig
  type t
  type id

  val resolve : id -> t Lwt.t
  (** [resolve id] returns scanner instance if already exists. *)

  val resolve_by_name : string -> t option Lwt.t
  (** [resolve_by_name name] returns filer instance by name. *)

  val store : t -> unit Lwt.t
  (** [store filer] stores [t] to any place. *)
end

(** Factory interface *)
module type Factory = sig
  type t

  val create : name:string -> file_tree:File_tree.t -> sort_order:Types.Sort_type.t -> t
  (** [create ~name ~file_tree ~sort_order] gets new instance of filer. *)
end
