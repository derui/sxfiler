open Sxfiler_core

type t =
  { location : Path.t
  ; items : File_item.t list }
[@@deriving eq, show, make]
(** [t] defines flat file tree of the location. File tree is entity in system *)

(** [has_same_location t1 t2] returns having same location between [t1] and [t2]. *)
let has_same_location t1 t2 = t1.location = t2.location

(** [find_item ~id t] returns the item having [id] given in [t] *)
let find_item ~id {items; _} = List.find_opt (fun v -> v.File_item.id = id) items

(* convert sort type to sort function *)
let to_sort_fun = function
  | Types.Sort_type.Date ->
      fun v1 v2 -> Pervasives.compare v1.File_item.stat.mtime v2.File_item.stat.mtime
  | Types.Sort_type.Name ->
      fun v1 v2 ->
        Pervasives.compare
          (Path.basename v1.File_item.full_path)
          (Path.basename v2.File_item.full_path)
  | Types.Sort_type.Size ->
      fun v1 v2 -> Pervasives.compare v1.File_item.stat.size v2.File_item.stat.size

(* sort items with sort_order in [t] *)
let sort_items t ~order =
  let sort_fun = to_sort_fun order in
  let dirs = List.filter File_item.is_directory t.items
  and files = List.filter (fun v -> not @@ File_item.is_directory v) t.items in
  {t with items = List.concat [List.sort sort_fun dirs; List.sort sort_fun files]}
