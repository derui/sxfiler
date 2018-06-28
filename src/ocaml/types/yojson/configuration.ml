(* [Configuration] module should be able to convert between json and ocaml.  *)
open Sxfiler_types.Configuration

module Key_maps = struct
  include Key_maps

  module Js = struct
    type t = {
      file_list: Yojson.Safe.json [@key "fileList"];
    } [@@deriving yojson]
  end

  let to_js : t -> Yojson.Safe.json = fun t ->
    Js.to_yojson {Js.file_list = Key_map.to_js t.file_list}

  let of_js : Yojson.Safe.json -> t = fun json ->
    match Js.of_yojson json with
    | Error _ -> Printf.eprintf "TODO parse error"; empty
    | Ok js -> {
        file_list = Key_map.of_js js.file_list;
      }
end

(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  include Viewer

  module Js = struct
    type t = {
      current_stack_name:string [@key "currentStackName"];
      stack_layout:string [@key "stackLayout"];
      key_maps:Yojson.Safe.json [@key "keyMaps"];
    } [@@deriving yojson]
  end

  let to_js : t -> Yojson.Basic.json = fun t ->
    let module T = Sxfiler_types.Types in
    let js = Js.{
        current_stack_name = t.current_stack_name;
        stack_layout = T.Layout.to_string t.stack_layout;
        key_maps = Key_maps.to_js t.key_maps;
      }
    in Js.to_yojson js |> Yojson.Safe.to_basic

  let of_js : Yojson.Safe.json -> t = fun json ->
    match Js.of_yojson json with
    | Error _ -> failwith "TODO parse error"
    | Ok js ->
      let module T = Sxfiler_types.Types in
      {
        current_stack_name = js.Js.current_stack_name;
        stack_layout = T.Layout.of_string js.Js.stack_layout;
        key_maps = Key_maps.of_js js.key_maps;
      }
end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  include Server

  module Js = struct
    type t = {
      sort_order:string [@key "sortOrder"];
    } [@@deriving yojson]
  end

  let to_js : t -> Yojson.Safe.json = fun t ->
    let module T = Sxfiler_types.Types in
    `Assoc [
      ("sortOrder", `String (T.Sort_type.to_string t.sort_order))
    ]

  let of_js : Yojson.Safe.json -> t = fun json ->
    match Js.of_yojson json with
    | Error _ -> failwith "TODO: handle parse error"
    | Ok js ->
      let module T = Sxfiler_types.Types in
      {
        sort_order = T.Sort_type.of_string js.Js.sort_order;
      }
end
