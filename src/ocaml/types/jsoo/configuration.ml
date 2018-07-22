(** This module provides function that is used to convert json to OCaml type,
    and extend original modules.
*)
open Sxfiler_types.Configuration

module Viewer = struct
  include Viewer

  class type js = object
    method currentStackName: Js.js_string Js.t Js.readonly_prop
    method stackLayout: Types.Layout.js Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      current_stack_name = Js.to_string js##.currentStackName;
      stack_layout = Types.Layout.of_js js##.stackLayout;
    }
end

module Server = struct
  include Server

  class type js = object
    method sortOrder: Types.Sort_type.js Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      sort_order = Types.Sort_type.of_js js##.sortOrder;
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
