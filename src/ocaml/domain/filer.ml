open Sxfiler_core
(** Scanner module provides type to scan file tree. *)

type id = Uuidm.t [@@deriving eq, show]

module Marked_item_set = struct
  include Set.Make (struct
    type t = File_item.id

    let compare = Stdlib.compare
  end)

  let pp fmt t =
    let ids = to_seq t |> List.of_seq |> List.sort Stdlib.compare in
    let printer = [%derive.show: File_item.id list] in
    Format.fprintf fmt "%s" @@ printer ids
end

type t =
  { id : id
  ; name : string
  ; file_list : File_list.t
  ; history : Location_history.t
  ; marked_items : Marked_item_set.t
  ; sort_order : Types.Sort_type.t }
[@@deriving eq, show, make]

let has_same_id {id = id1; _} {id = id2; _} = equal_id id1 id2
let find_item t = File_list.find_item t.file_list

let update_list t ~file_list =
  let marked_items =
    if Path.equal file_list.File_list.location t.file_list.location then
      let current_items =
        List.map File_item.id file_list.File_list.items |> Marked_item_set.of_list
      in
      Marked_item_set.inter t.marked_items current_items
    else Marked_item_set.empty
  in
  {t with file_list = File_list.sort_items file_list ~order:t.sort_order; marked_items}

let move_location t ~file_list clock =
  let record = Location_record.record_of ~location:file_list.File_list.location clock in
  let history = Location_history.add_record t.history ~record in
  let t' = update_list t ~file_list in
  {t' with history}

let add_mark t ~ids =
  let marked_items =
    List.fold_left (fun set id -> Marked_item_set.add id set) t.marked_items ids
  in
  {t with marked_items}

let remove_mark t ~ids =
  let marked_items =
    List.fold_left (fun set id -> Marked_item_set.remove id set) t.marked_items ids
  in
  {t with marked_items}

module type Repository = Filer_intf.Repository with type t := t and type id := id

(** Factory interface *)
module Factory = struct
  module type S = Filer_intf.Factory with type t := t

  module Make (G : Id_generator_intf.Gen_random with type id = id) : S = struct
    let create ~name ~file_list ~sort_order =
      let file_list = File_list.sort_items ~order:sort_order file_list in
      make ~id:(G.generate ()) ~name ~file_list ~sort_order ~history:(Location_history.make ())
        ~marked_items:Marked_item_set.empty
  end
end
