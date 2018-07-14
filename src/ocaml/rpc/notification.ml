module T = Sxfiler_types
module Workspace_update = struct
  type params = {
    name: string;
    workspace: T.Workspace.t;
  }

  type result = unit
  let name = "notification/workspace/update"
end

module Scanner_update = struct
  type params = {
    name: string;
    scanner: T.Scanner.t;
  }

  type result = unit
  let name = "notification/scanner/update"
end
