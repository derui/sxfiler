(** [Key_maps] defines key mappings that are used in each view. *)
open Sxfiler_types.Configuration

module Key_maps = struct
  include Key_maps

  let to_js : t -> Yojson.json = fun t ->
    `Assoc [("fileList", Key_map.to_js t.file_list)]
end

(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  include Viewer

  let to_js : t -> Yojson.json = fun t ->
    let module T = Sxfiler_types.Types in
    `Assoc [
      ("currentStackName", `String t.current_stack_name);
      ("stackLayout", `String (T.Layout.to_string t.stack_layout));
      ("keyMaps", Key_maps.to_js t.key_maps);
    ]
end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  include Server

  let to_js : t -> Yojson.json = fun t ->
    let module T = Sxfiler_types.Types in
    `Assoc [
      ("sortOrder", `String (T.Sort_type.to_string t.sort_order))
    ]
end
