(** Node is a item in a file tree. *)
include Sxfiler_types.Node

class type js = object
  method fullPath: Js.js_string Js.t Js.readonly_prop
  method stat: File_stat.js Js.t Js.readonly_prop
  method parentDirectory: Js.js_string Js.t Js.readonly_prop
  method linkPath: Js.js_string Js.t Js.opt Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    full_path = Js.to_string js##.fullPath;
    stat = File_stat.of_js js##.stat;
    parent_directory = Js.to_string js##.parentDirectory;
    link_path = Js.Opt.map js##.linkPath Js.to_string |> Js.Opt.to_option;
  }
