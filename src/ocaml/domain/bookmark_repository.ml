open Sxfiler_core

module type S = sig
  val store : Bookmark.t -> unit Lwt.t
  (** [store t] stores a bookmark into repository *)

  val find_all : unit -> Bookmark.t list Lwt.t
  (** [find_all ()] gets all bookmarks in repository *)

  val resolve : Bookmark.id -> Bookmark.t option Lwt.t
  (** [resolve id] resolve a bookmark from the id *)

  val find_by_path : Path.t -> Bookmark.t option Lwt.t
  (** [find_by_path path] find a bookmark from the id *)
end
