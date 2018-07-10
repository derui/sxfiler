include Sxfiler_types.Workspace

module Js = struct
  type t = {
    current: Tree_snapshot.t;
    history: Snapshot_history.t;
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson {
    Js.current = t.current;
    history = t.history;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    current = v.Js.current;
    history = v.Js.history;
  }
