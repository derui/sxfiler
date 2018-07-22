(* [Configuration] module should be able to convert between json and ocaml.  *)
open Sxfiler_types.Configuration

(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  include Viewer

  module Js = struct
    type t = {
      current_stack_name:string [@key "currentStackName"];
      stack_layout:Types.Layout.t [@key "stackLayout"];
    } [@@deriving yojson]
  end

  let to_yojson : t -> Yojson.Safe.json = fun t ->
    let module T = Sxfiler_types.Types in
    let js = {
      Js.current_stack_name = t.current_stack_name;
      stack_layout = t.stack_layout;
    }
    in Js.to_yojson js

  let of_yojson json =
    let open Ppx_deriving_yojson_runtime in
    Js.of_yojson json >>= fun js ->
    Ok {
      current_stack_name = js.Js.current_stack_name;
      stack_layout = js.Js.stack_layout;
    }
end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  include Server

  module Js = struct
    type t = {
      sort_order:Types.Sort_type.t [@key "sortOrder"];
    } [@@deriving yojson]
  end

  let to_yojson : t -> Yojson.Safe.json = fun t -> Js.to_yojson {Js.sort_order = t.sort_order}

  let of_yojson json =
    let open Ppx_deriving_yojson_runtime in
    Js.of_yojson json >>= fun js ->
    Ok {sort_order = js.Js.sort_order}
end

module Js = struct
  type t = {
    server: Yojson.Safe.json;
    viewer: Yojson.Safe.json;
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson {
    Js.viewer = Viewer.to_yojson t.viewer;
    server = Server.to_yojson t.server;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun js ->
  Server.of_yojson js.server >>= fun server ->
  Viewer.of_yojson js.viewer >>= fun viewer ->
  Ok {server;viewer}
