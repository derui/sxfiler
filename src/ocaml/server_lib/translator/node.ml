(** This module defines translator for {Node} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module D = Sxfiler_domain.Node

type t = {
  full_path : string [@key "fullPath"];
  stat: File_stat.t;
  parent_directory: string [@key "parentDirectory"];
  link_path: string option [@key "linkPath"]
} [@@deriving yojson]

let of_domain t = {
  full_path = Path.to_string t.D.full_path;
  stat = File_stat.of_domain t.stat;
  parent_directory = t.parent_directory;
  link_path = t.link_path;
}

let to_domain ?(system=(module System.Real:System.S)) t =
  let module S = (val system) in
  {
    D.full_path = Path.of_string (module S) t.full_path;
    stat = File_stat.to_domain t.stat;
    parent_directory = t.parent_directory;
    link_path = t.link_path;
  }
