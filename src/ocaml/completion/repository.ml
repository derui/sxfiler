open Domain

(** Repository interface *)
module type Collection = sig
  (** [store collection] saves collection to some space. *)
  val store: collection -> unit Lwt.t

  (** [resolve ()] returns collection that is stored now.  *)
  val resolve: unit -> collection Lwt.t
end
