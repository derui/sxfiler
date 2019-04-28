(** Scanner module provides type to scan file tree. *)

type id = string [@@deriving eq, show]

module Node_id_set = struct
  include Set.Make (struct
    type t = Node.id

    let compare = Stdlib.compare
  end)

  let pp fmt t =
    let ids = to_seq t |> List.of_seq |> List.sort Stdlib.compare in
    let printer = [%derive.show: Node.id list] in
    Format.fprintf fmt "%s" @@ printer ids
end

type t =
  { id : id
  ; file_tree : File_tree.t
  ; history : Location_history.t
  ; marked_nodes : Node_id_set.t
  ; sort_order : Types.Sort_type.t }
[@@deriving eq, show, make]

let has_same_id {id = id1; _} {id = id2; _} = equal_id id1 id2
let find_node t = File_tree.find_node t.file_tree

let move_location t ~file_tree clock =
  let record = Location_record.record_of ~location:file_tree.File_tree.location clock in
  let history = Location_history.add_record t.history ~record in
  {t with file_tree = File_tree.sort_nodes file_tree ~order:t.sort_order; history}

let update_tree t ~file_tree =
  {t with file_tree = File_tree.sort_nodes file_tree ~order:t.sort_order}

let add_mark t ~ids =
  let marked_nodes = List.fold_left (fun set id -> Node_id_set.add id set) t.marked_nodes ids in
  {t with marked_nodes}

let remove_mark t ~ids =
  let marked_nodes = List.fold_left (fun set id -> Node_id_set.remove id set) t.marked_nodes ids in
  {t with marked_nodes}

let node_subset t ~ids =
  List.fold_left
    (fun (nodes, ids) id ->
      match File_tree.find_node t.file_tree ~id with
      | None -> (nodes, id :: ids)
      | Some node -> (node :: nodes, ids) )
    ([], []) ids

(** Signature for repository of scanner. *)
module type Repository = sig
  val resolve : string -> t option Lwt.t
  (** [resolve id] returns scanner instance if already exists. *)

  val store : t -> unit Lwt.t
  (** [store filer] stores [t] to any place. *)
end

module Factory = struct
  module type S = sig
    val create :
         name:string
      -> file_tree:File_tree.t
      -> history:Location_history.t
      -> sort_order:Types.Sort_type.t
      -> t
    (** [create ~file_tree ~history ~sort_order] gets new instance of filer. *)
  end

  module Make : S = struct
    let create ~name ~file_tree ~history ~sort_order =
      { id = name
      ; file_tree = File_tree.sort_nodes file_tree ~order:sort_order
      ; history
      ; sort_order
      ; marked_nodes = Node_id_set.empty }
  end
end
