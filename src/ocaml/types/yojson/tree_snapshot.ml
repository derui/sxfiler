include Sxfiler_types.Tree_snapshot

module Js = struct
  type t = {
    directory: string;
    nodes: Node.t list;
  } [@@deriving yojson]
end

(** [to_yojson t] converts {!type:t} to {!type:Yojson.json}. *)
let to_yojson : t -> Yojson.Safe.json = fun t -> Js.to_yojson {
    Js.directory = t.directory;
    nodes = t.nodes;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    directory = v.Js.directory;
    nodes = v.Js.nodes;
  }
