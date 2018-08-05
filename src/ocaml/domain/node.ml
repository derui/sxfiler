(** Node is a domain in a file tree. *)
open Sxfiler_core

(** [link_path] will have target of the link if item pointed to full_path is *link. *)
type t = {
  full_path: Path.t;
  stat: File_stat.t;
  parent_directory: string;
  link_path: string option;
}

let equal v1 v2 = v1.full_path = v2.full_path

let make ~full_path ~stat ~parent_directory ~link_path =
  {
    full_path;
    stat;
    parent_directory;
    link_path
  }

module type Repository = sig
  (** [find_by_dir ~dir] search nodes in directory. *)
  val find_by_dir: dir:Path.t -> t list Lwt.t
end
