(** This module provides function that is used to convert json to OCaml type,
    and extend original modules.
*)
open Sxfiler_types.Configuration

module Key_maps = struct
  include Key_maps
  class type js = object
    method default: < > Js.t Js.readonly_prop
    method fileList: < > Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      default = Key_map.of_js js##.default;
      file_list = Key_map.of_js js##.fileList;
    }
end

module Viewer = struct
  include Viewer

  class type js = object
    method currentStackName: Js.js_string Js.t Js.readonly_prop
    method stackLayout: Js.js_string Js.t Js.readonly_prop
    method keyMaps: Key_maps.js Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      current_stack_name = Js.to_string js##.currentStackName;
      stack_layout = Js.to_string js##.stackLayout |> Sxfiler_types.Types.Layout.of_string;
      key_maps = Key_maps.of_js js##.keyMaps;
    }
end

module Server = struct
  include Server

  class type js = object
    method sortOrder: Js.js_string Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      sort_order = Js.to_string js##.sortOrder |> Sxfiler_types.Types.Sort_type.of_string;
    }
end

class type js = object
  method viewer: Viewer.js Js.t Js.readonly_prop
  method server: Server.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    viewer = Viewer.of_js js##.viewer;
    server = Server.of_js js##.server;
  }
