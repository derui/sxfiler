(** This module defines translator for {Scanner} module to translate from domain to
    outer model.
*)
open Sxfiler_core
module D = Sxfiler_domain.Scanner

type t = {
  id: string;
  location: string;
  nodes: Node.t list;
  history: Location_history.t;
} [@@deriving yojson]

let of_domain t = {
  id = t.D.id;
  location = Path.to_string t.location;
  nodes = List.map Node.of_domain t.nodes;
  history = Location_history.of_domain t.history;
}

let to_domain ?(system=(module System.Real:System.S)) t =
  let module S = (val system) in
  {
  D.id = t.id;
  location = Path.of_string (module S) t.location;
  nodes = List.map Node.to_domain t.nodes;
  history = Location_history.to_domain t.history;
}
