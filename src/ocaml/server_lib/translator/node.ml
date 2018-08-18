(** This module defines translator for {Node} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module D = Sxfiler_domain.Node

type t = {
  name : string [@key "name"];
  stat: File_stat.t;
  parent_directory: string [@key "parentDirectory"];
  link_path: string option [@key "linkPath"]
} [@@deriving yojson]

let of_domain t = {
  name = Path.to_string t.D.full_path;
  stat = File_stat.of_domain t.stat;
  parent_directory = t.parent_directory;
  link_path = t.link_path;
}

let to_domain t =
  {
    D.full_path = Path.of_list [t.parent_directory;t.name];
    stat = File_stat.to_domain t.stat;
    parent_directory = t.parent_directory;
    link_path = t.link_path;
  }
