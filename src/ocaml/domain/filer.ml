(** Scanner module provides type to scan file tree. *)

open Sxfiler_core

type id = string [@@deriving show]

type t =
  { id : id
  ; location : Path.t
  ; nodes : Node.t list
  ; history : Location_history.t
  ; selected_nodes : Node.id list
  ; sort_order : Types.Sort_type.t }
[@@deriving show]

(* convert sort type to sort function *)
let to_sort_fun = function
  | Types.Sort_type.Date ->
    fun v1 v2 -> Pervasives.compare v1.Node.stat.File_stat.mtime v2.Node.stat.File_stat.mtime
  | Types.Sort_type.Name ->
    fun v1 v2 ->
      Pervasives.compare (Path.basename v1.Node.full_path) (Path.basename v2.Node.full_path)
  | Types.Sort_type.Size ->
    fun v1 v2 -> Pervasives.compare v1.Node.stat.File_stat.size v2.Node.stat.File_stat.size

(* sort nodes with sort_order in [t] *)
let sort_nodes t =
  let sort_fun = to_sort_fun t.sort_order in
  let dirs = List.filter Node.is_directory t.nodes
  and files = List.filter (fun v -> not @@ Node.is_directory v) t.nodes in
  {t with nodes = List.concat [List.sort sort_fun dirs; List.sort sort_fun files]}

let move_location t ~location ~nodes clock =
  let record = Location_record.record_of ~location clock in
  let history = Location_history.add_record t.history ~record in
  sort_nodes {t with location; nodes; history}

(** [make ~id ~location ~nodes ~history ~sort_order] gets new instance of filer. *)
let make ~id ~location ~nodes ~history ~sort_order =
  let t = {id; location; nodes; history; sort_order; selected_nodes = []} in
  sort_nodes t

(** [update_nodes t ~nodes] get new filer is based on [t] and updated nodes from parameter. *)
let update_nodes t ~nodes =
  let t = {t with nodes} in
  sort_nodes t

(** [find_node t ~id] search node having [id] in filer [t] *)
let find_node t ~id = List.find_opt (fun (v : Node.t) -> v.id = id) t.nodes

module Node_id_set = Set.Make (struct
    type t = Node.id

    let compare = Stdlib.compare
  end)

let select_nodes t ~ids =
  let selected_set = Node_id_set.of_list t.selected_nodes in
  let selected_nodes =
    List.fold_left (fun set id -> Node_id_set.add id set) selected_set ids
    |> Node_id_set.to_seq |> List.of_seq
  in
  {t with selected_nodes}

let deselect_nodes t ~ids =
  let selected_nodes = List.filter (fun id -> not @@ List.mem id ids) t.selected_nodes in
  {t with selected_nodes}

let node_subset t ~ids =
  List.fold_left
    (fun (nodes, ids) id ->
       match find_node t ~id with None -> (nodes, id :: ids) | Some node -> (node :: nodes, ids) )
    ([], []) ids

(** Signature for repository of scanner. *)
module type Repository = sig
  val resolve : string -> t option Lwt.t
  (** [resolve id] returns scanner instance if already exists. *)

  val store : t -> unit Lwt.t
  (** [store filer] stores [t] to any place. *)
end
