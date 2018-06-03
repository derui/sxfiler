(** This module provides some of converters for Sxfiler_common defined.
    Because Sxfiler_common provides only js_of_ocaml integration to reduce size of the compilied
    JavaScript, but server will use yojson and not use js_of_ocaml.
*)
module C = Sxfiler_common

module Key_map = struct
  type t = C.Key_map.t
  let to_json t =
    let key_values = C.Key_map.Key_map.bindings t in
    let assocs = List.map
        (fun (key, value) -> `Assoc (key, `String (C.Callable_action.to_string value))) key_values in
    `Assoc assocs
end

module Config = struct

  module Key_maps = struct
    let to_json t =
      `Assoc [("fileList", Key_map.to_json t.C.Config.Key_maps.file_list)]
  end

  let to_json t =
    `Assoc [("keyMaps", Key_maps.to_json t.C.Config.key_maps)]
end
