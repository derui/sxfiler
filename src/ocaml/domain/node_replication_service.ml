(** this module defines interface to replicate node between filers.
    No implementation that is default provides from this, should implement yourself.
*)

module type S = sig
  val replicate : ?new_name:string -> node:Node.t -> _to:File_tree.t -> unit -> unit Lwt.t
  (** [replicate ?new_name ~node ~_to ()] replicate [node] to the location [_to].
      When [new_name] gives to this function, replicate as [new_name] to the location. If [new_name] not given and
      node that has same name of [node] exists in the location [_to], overwrite it.
  *)
end
