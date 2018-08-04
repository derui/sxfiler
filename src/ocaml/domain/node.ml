(** Node is a item in a file tree. *)

(** [link_path] will have target of the link if item pointed to full_path is *link. *)
type t = {
  full_path: string;
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
