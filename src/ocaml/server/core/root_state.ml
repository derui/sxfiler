module T = Sxfiler_types

module Workspace_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

type t = {
  configuration: T.Configuration.t;
  workspace_map: T.Workspace.t Workspace_map.t;
}

let empty = {
  configuration = T.Configuration.default;
  workspace_map = Workspace_map.empty;
}

let find_workspace ~name t = Workspace_map.find_opt name t.workspace_map
