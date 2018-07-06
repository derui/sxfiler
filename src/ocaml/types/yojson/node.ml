(** Node is a item in a file tree. *)
include Sxfiler_types.Node

module Js = struct
  type t = {
    full_path : string [@key "fullPath"];
    stat: File_stat.t;
    parent_directory: string [@key "parentDirectory"];
    link_path: string option [@key "linkPath"]
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t -> Js.to_yojson Js.{
    full_path = t.full_path;
    stat = t.stat;
    parent_directory = t.parent_directory;
    link_path = t.link_path;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    full_path = v.Js.full_path;
    stat = v.Js.stat;
    parent_directory = v.Js.parent_directory;
    link_path = v.Js.link_path;
  }
