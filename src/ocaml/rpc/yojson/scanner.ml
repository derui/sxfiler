module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

module Make_sync = struct
  open Rpc.Scanner.Make_sync

  module Js = struct
    type params = {
      initial_location: string [@key "initialLocation"];
      name: string;
    } [@@deriving yojson]

  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {
      initial_location = js.Js.initial_location;
      name = js.Js.name;
    }

  let params_to_yojson : params -> Yojson.Safe.json = fun t -> Js.params_to_yojson {
      Js.initial_location = t.initial_location;
      name = t.name;
    }
end

module Get_sync = struct
  open Rpc.Scanner.Get_sync
  module Js = struct
    type params = {
      name: string;
    } [@@deriving yojson]

  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {name = js.Js.name;}

  let result_to_yojson = Ty.Scanner.to_yojson
end