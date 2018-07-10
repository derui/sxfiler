module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

module Make_sync = struct
  open Rpc.Workspace.Make_sync

  module Js = struct
    type params = {
      initial_directory: string [@key "initialDirectory"];
      name: string;
    } [@@deriving yojson]

    type result = {
      created: bool;
    } [@@deriving yojson]
  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {
      initial_directory = js.Js.initial_directory;
      name = js.Js.name;
    }

  let params_to_yojson : params -> Yojson.Safe.json = fun t -> Js.params_to_yojson {
      Js.initial_directory = t.initial_directory;
      name = t.name;
    }

  let result_to_yojson : result -> Yojson.Safe.json = fun t -> Js.result_to_yojson {
      Js.created = t.created
    }
end

module Get_sync = struct
  open Rpc.Workspace.Get_sync
  module Js = struct
    type params = {
      name: string;
    } [@@deriving yojson]

  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {name = js.Js.name;}

  let result_to_yojson = Ty.Workspace.to_yojson
end
