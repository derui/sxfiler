(** This module provides function that is used to convert json to OCaml type,
    and extend original modules.
*)
open Sxfiler_domain.Configuration

module Viewer = struct
  open Viewer

  class type js = object
    method currentStackName: Js.js_string Js.t Js.readonly_prop
    method stackLayout: Types.Layout.js Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      current_stack_name = Js.to_string js##.currentStackName;
      stack_layout = Types.Layout.of_js js##.stackLayout;
    }

  let to_js t : js Js.t = object%js
    val currentStackName = Js.string t.current_stack_name
    val stackLayout = Types.Layout.to_js t.stack_layout
  end
end

module Server = struct
  open Server

  class type js = object
    method sortOrder: Types.Sort_type.js Js.t Js.readonly_prop
  end

  let of_js : js Js.t -> t = fun js ->
    {
      sort_order = Types.Sort_type.of_js js##.sortOrder;
    }

  let to_js t : js Js.t = object%js
    val sortOrder = Types.Sort_type.to_js t.sort_order
  end
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

let to_js t : js Js.t = object%js
  val viewer = Viewer.to_js t.viewer
  val server = Server.to_js t.server
end
