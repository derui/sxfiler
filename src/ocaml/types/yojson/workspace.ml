include Sxfiler_types.Workspace

module Js = struct
  type t = {
    source: Tree_snapshot.t;
    target: Tree_snapshot.t;
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson {
    Js.source = t.source;
    target = t.target;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    source = v.Js.source;
    target = v.Js.target;
  }
