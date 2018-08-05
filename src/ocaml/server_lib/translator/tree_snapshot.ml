module D = Sxfiler_domain.Tree_snapshot

type t = {
  directory: string;
  nodes: Node.t list;
} [@@deriving yojson]

(** [of_domain t] converts {!type:D.t} to {!type:t}. *)
let of_domain t = {
  directory = t.D.directory;
  nodes = List.map Node.of_domain t.nodes;
}
