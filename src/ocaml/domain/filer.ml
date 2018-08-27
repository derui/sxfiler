(** Scanner module provides type to scan file tree. *)

open Sxfiler_core

type t = {
  id: string;
  location: Path.t;
  nodes: Node.t list;
  history: Location_history.t;
  sort_order: Types.Sort_type.t;
}

(* convert sort type to sort function *)
let to_sort_fun = function
  | Types.Sort_type.Date -> fun v1 v2 -> Pervasives.compare v1.Node.stat.File_stat.mtime v2.Node.stat.File_stat.mtime
  | Types.Sort_type.Name -> fun v1 v2 -> Pervasives.compare (Path.basename v1.Node.full_path) (Path.basename v2.Node.full_path)
  | Types.Sort_type.Size -> fun v1 v2 -> Pervasives.compare v1.Node.stat.File_stat.size v2.Node.stat.File_stat.size

(* sort nodes with sort_order in [t] *)
let sort_nodes t =
  let sort_fun = to_sort_fun t.sort_order in
  let dirs = List.filter Node.is_directory t.nodes
  and files = List.filter (fun v -> not @@ Node.is_directory v) t.nodes in
  {t with nodes = List.concat [
       List.sort sort_fun dirs;
       List.sort sort_fun files;
     ]
  }

let move_location t ~location ~nodes clock =
  let record = Location_record.record_of ~location clock in
  let history = Location_history.add_record t.history ~record in
  sort_nodes {t with location; nodes; history}

(** [make ~id ~location ~nodes ~history ~sort_order] gets new instance of filer. *)
let make ~id ~location ~nodes ~history ~sort_order =
  let t = {
    id;
    location;
    nodes;
    history;
    sort_order;
  } in
  sort_nodes t

(** Signature for repository of scanner. *)
module type Repository = sig
  (** [resolve id] returns scanner instance if already exists. *)
  val resolve: string -> t option Lwt.t

  (** [store scanner] stores [t] to any place. *)
  val store: t -> unit Lwt.t
end
