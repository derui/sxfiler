module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

module Init_sync = struct
  include Rpc.Command.Init_sync

  module Js = struct
    type params = {
      command: Ty.Command.Class.t;
    } [@@deriving yojson]
  end

  let params_of_yojson t =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson t >>= fun v ->
    Ok {command = v.Js.command}
end

module Param_update = struct
  include Rpc.Command.Param_update.Make(struct
      type t = Yojson.Safe.json
    end)

  module Js = struct
    type params = {
      name: string;
      value: Yojson.Safe.json;
    } [@@deriving yojson]
  end

  let params_of_yojson t =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson t >>= fun v ->
    Ok {name = v.Js.name; value = v.Js.value}
end
