open Sxfiler_core
(** Item is a domain in a file tree. *)

type id = string [@@deriving eq, show]
(** identifier in file tree. *)

type t = {
  id : id;
  full_path : Path.t;
  stat : File_stat.t;
  link_path : Path.t option;
}
[@@deriving eq, show, fields]
(** [link_path] will have target of the link if item pointed to full_path is *link. *)

let has_same_id v1 v2 = equal_id v1.id v2.id
let make ~id ~full_path ~stat ~link_path = { id; full_path; stat; link_path }

(** [is_directory v] shortcut function to detect item is directory or not *)
let is_directory v = v.stat.File_stat.is_directory
