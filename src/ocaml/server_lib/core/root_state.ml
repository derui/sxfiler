module T = Sxfiler_domain

module String_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

type t = {
  configuration: T.Configuration.t;
  workspace: T.Workspace.t option;
  scanner_map: T.Scanner.t String_map.t;
}

let empty = {
  configuration = T.Configuration.default;
  workspace = None;
  scanner_map = String_map.empty;
}

let find_scanner ~name t = String_map.find_opt name t.scanner_map

let add_scanner ~scanner t = {
  t with scanner_map = String_map.add scanner.T.Scanner.name scanner t.scanner_map;
}
