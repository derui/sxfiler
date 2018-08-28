open Domain

(** Repository interface *)
module type Collection = sig
  val store : collection -> unit Lwt.t
  (** [store collection] saves collection to some space. *)

  val resolve : unit -> collection Lwt.t
  (** [resolve ()] returns collection that is stored now.  *)
end
