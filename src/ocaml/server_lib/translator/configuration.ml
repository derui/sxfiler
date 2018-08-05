(** This module defines translator for {Configuration} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module T = Sxfiler_domain

module Sort_type = struct
  type t = T.Types.Sort_type.t

  let of_yojson = function
    | `Int v -> begin match v with
        | 1 -> Ok T.Types.Sort_type.Date
        | 2 -> Ok T.Types.Sort_type.Name
        | 3 -> Ok T.Types.Sort_type.Size
        | _ -> Error "Unknown sort_type"
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (match t with
      | T.Types.Sort_type.Date -> 1
      | T.Types.Sort_type.Name -> 2
      | T.Types.Sort_type.Size -> 3)

  let of_domain = Fun.ident
  let to_domain = Fun.ident
end

module Layout = struct
  type t = T.Types.Layout.t

  let of_yojson js =
    match js with
    | `Int v -> begin match v with
        | 1 -> Ok T.Types.Layout.Side_by_side
        | _ -> Error "Unknown type"
      end
    | _ -> Error "Invalid JSON type"

  let to_yojson t = `Int (match t with
      | T.Types.Layout.Side_by_side -> 1)

  let of_domain = Fun.ident
  let to_domain = Fun.ident
end

(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  type t = {
    current_stack_name:string [@key "currentStackName"];
    stack_layout: Layout.t [@key "stackLayout"];
  } [@@deriving yojson]

  let of_domain t = {
    current_stack_name = t.T.Configuration.Viewer.current_stack_name;
    stack_layout = Layout.of_domain t.stack_layout;
  }

  let to_domain t = {
    T.Configuration.Viewer.current_stack_name = t.current_stack_name;
    stack_layout = Layout.to_domain t.stack_layout;
  }
end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  type t = {
    sort_order: Sort_type.t [@key "sortOrder"];
  } [@@deriving yojson]

  let of_domain t = {
    sort_order = Sort_type.of_domain t.T.Configuration.Server.sort_order;
  }

  let to_domain t = {
    T.Configuration.Server.sort_order = Sort_type.to_domain t.sort_order;
  }
end

type t = {
  server: Server.t;
  viewer: Viewer.t;
} [@@deriving yojson]

let of_domain t = {
  server = Server.of_domain t.T.Configuration.server;
  viewer = Viewer.of_domain t.T.Configuration.viewer;
}

let to_domain t = {
  T.Configuration.server = Server.to_domain t.server;
  T.Configuration.viewer = Viewer.to_domain t.viewer;
}
