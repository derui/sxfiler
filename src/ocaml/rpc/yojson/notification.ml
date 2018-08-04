module T = Sxfiler_domain
module Ty = Sxfiler_domain_yojson
module Rpc = Sxfiler_rpc

module Workspace_update = struct
  open Rpc.Notification.Workspace_update

  module Js = struct
    type params = {
      name: string;
      workspace: Ty.Workspace.t;
    } [@@deriving yojson]
  end

  let params_to_yojson : params -> Yojson.Safe.json = fun t -> Js.params_to_yojson {
      Js.name = t.name;
      workspace = t.workspace;
    }

end

module Scanner_update = struct
  open Rpc.Notification.Scanner_update

  module Js = struct
    type params = {
      name: string;
      scanner: Ty.Scanner.t;
    } [@@deriving yojson]
  end

  let params_to_yojson : params -> Yojson.Safe.json = fun t -> Js.params_to_yojson {
      Js.name = t.name;
      scanner = t.scanner;
    }

end
