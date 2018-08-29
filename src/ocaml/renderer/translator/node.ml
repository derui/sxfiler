(** Node is a item in a file tree. *)
open Sxfiler_core

include Sxfiler_rpc.Types.Node

class type js =
  object
    method id : Js.js_string Js.t Js.readonly_prop

    method name : Js.js_string Js.t Js.readonly_prop

    method stat : File_stat.js Js.t Js.readonly_prop

    method parentDirectory : Js.js_string Js.t Js.readonly_prop

    method linkPath : Js.js_string Js.t Js.opt Js.readonly_prop
  end

let of_js js : t =
  (* full_path should be absolute path. *)
  { id = Js.to_string js##.id
  ; name = Js.to_string js##.name
  ; stat = File_stat.of_js js##.stat
  ; parent_directory = Js.to_string js##.parentDirectory
  ; link_path = Js.Opt.map js##.linkPath Js.to_string |> Js.Opt.to_option }


let to_js t : js Js.t =
  object%js
    val id = Js.string t.id

    val name = Js.string t.name

    val stat = File_stat.to_js t.stat

    val parentDirectory = Js.string t.parent_directory

    val linkPath = Option.fmap ~f:Js.string t.link_path |> Js.Opt.option
  end
