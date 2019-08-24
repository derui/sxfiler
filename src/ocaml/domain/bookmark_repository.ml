open Sxfiler_core

module type S = sig
  val store : Bookmark.t -> unit
  (** [store t] stores a bookmark into repository *)

  val resolve : Bookmark.id -> Bookmark.t option
  (** [resolve id] resolve a bookmark from the id *)

  val find_by_path : Path.t -> Bookmark.t option
  (** [find_by_path path] find a bookmark from the id *)
end
