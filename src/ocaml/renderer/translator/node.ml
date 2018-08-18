(** Node is a item in a file tree. *)
open Sxfiler_core
include Sxfiler_domain.Node

class type js = object
  method fullPath: Js.js_string Js.t Js.readonly_prop
  method stat: File_stat.js Js.t Js.readonly_prop
  method parentDirectory: Js.js_string Js.t Js.readonly_prop
  method linkPath: Js.js_string Js.t Js.opt Js.readonly_prop
end

let of_js js : t =
  {
    (* full_path should be absolute path. *)
    full_path = Path.of_string @@ Js.to_string js##.fullPath;
    stat = File_stat.of_js js##.stat;
    parent_directory = Js.to_string js##.parentDirectory;
    link_path = Js.Opt.map js##.linkPath Js.to_string |> Js.Opt.to_option;
  }


let to_js t : js Js.t = object%js
  val fullPath = Js.string @@ Path.to_string t.full_path
  val stat = File_stat.to_js t.stat
  val parentDirectory = Js.string t.parent_directory
  val linkPath = Option.fmap ~f:Js.string t.link_path |> Js.Opt.option
end
