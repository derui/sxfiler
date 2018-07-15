include Sxfiler_types.Scanner

module Js = struct
  type t = {
    name: string;
    location: string;
    nodes: Node.t list;
    history: Location_history.t;
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson {
    Js.name = t.name;
    location = t.location;
    nodes = t.nodes;
    history = t.history;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    name = v.Js.name;
    location = v.Js.location;
    nodes = v.Js.nodes;
    history = v.Js.history;
  }
