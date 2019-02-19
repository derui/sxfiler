open Sxfiler_core

(** [t] defines flat file tree of the location. File tree is entity in system *)
type t =
  { location : Path.t
  ; nodes : Node.t list }
[@@deriving eq, show]

(** [make ~location ~nodes] returns new instance of [t] *)
let make ~location ~nodes = {location; nodes}

(** [is_same_location t1 t2] returns having same location between [t1] and [t2]. *)
let is_same_location t1 t2 = t1.location = t2.location

(** [find_node ~id t] returns the node having [id] given in [t] *)
let find_node ~id {nodes; _} = List.find_opt (fun v -> v.Node.id = id) nodes

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
let sort_nodes t ~order =
  let sort_fun = to_sort_fun order in
  let dirs = List.filter Node.is_directory t.nodes
  and files = List.filter (fun v -> not @@ Node.is_directory v) t.nodes in
  {t with nodes = List.concat [List.sort sort_fun dirs; List.sort sort_fun files]}
