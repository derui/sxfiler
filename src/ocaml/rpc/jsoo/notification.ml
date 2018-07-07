module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo
module Rpc = Sxfiler_rpc

module Workspace_update = struct
  open Rpc.Notification.Workspace_update

  class type js_params = object
    method name: Js.js_string Js.t Js.readonly_prop
    method workspace: Tj.Workspace.js Js.t Js.readonly_prop
  end

  let params_of_json js = {
    name = Js.to_string js##.name;
    workspace = Tj.Workspace.of_js js##.workspace;
  }
end
