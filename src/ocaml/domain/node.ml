(** Node is a domain in a file tree. *)
open Sxfiler_core

(** [link_path] will have target of the link if item pointed to full_path is *link. *)
type t =
  { id : string
  ; full_path : Path.t
  ; stat : File_stat.t
  ; link_path : string option }

let equal v1 v2 = v1.id = v2.id
let make ~id ~full_path ~stat ~link_path = {id; full_path; stat; link_path}

(** [is_directory v] shortcut function to detect node is directory or not *)
let is_directory v = v.stat.File_stat.is_directory
