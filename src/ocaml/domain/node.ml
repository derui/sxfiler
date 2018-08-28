(** Node is a domain in a file tree. *)
open Sxfiler_core

(** [link_path] will have target of the link if item pointed to full_path is *link. *)
type t =
  { full_path : Path.t
  ; stat : File_stat.t
  ; link_path : string option }

let equal v1 v2 = v1.full_path = v2.full_path
let make ~full_path ~stat ~link_path = {full_path; stat; link_path}

(** [is_directory v] shortcut function to detect node is directory or not *)
let is_directory v = v.stat.File_stat.is_directory

module type Repository = sig
  val find_by_dir : dir:Path.t -> t list Lwt.t
  (** [find_by_dir ~dir] search nodes in directory. *)
end
