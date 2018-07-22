(** This module defines RPC interface for command operation.*)

module Rpc = Sxfiler_rpc
module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo

module Init_sync = struct
  open Rpc.Command.Init_sync

  class type js_params = object
    method command: Tj.Command_intf.Class.js Js.t Js.readonly_prop
  end

  let params_of_json js = {
    command = Tj.Command_intf.Class.of_js js##.command;
  }
end

module Param_update = struct
  include Rpc.Command.Param_update.Make(struct
      type t = < > Js.t
    end)

  class type js_params = object
    method name: Js.js_string Js.t Js.readonly_prop
    method value: < > Js.t Js.readonly_prop
  end

  let params_of_json js = {
    name = Js.to_string js##.name;
    value = js##.value;
  }
end
