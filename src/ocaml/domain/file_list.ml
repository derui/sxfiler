open Sxfiler_core
(** Scanner module provides type to scan file tree. *)

module Id = Common.Identity.Make (struct
  type t = string [@@deriving eq, show, ord]
end)

module Item_map = struct
  include Map.Make (struct
    type t = File_item.Id.t

    let compare = File_item.Id.compare
  end)
end

type scanned =
  | Valid       of {
      id : Id.t;
      location : Path.t;
      items : File_item.t list;
      sort_order : Types.Sort_type.t;
    }
  | No_location of {
      id : Id.t;
      location : Path.t;
      sort_order : Types.Sort_type.t;
    }
[@@deriving eq, show]

type unscanned = {
  id : Id.t;
  location : Path.t;
  sort_order : Types.Sort_type.t;
}
[@@deriving eq, show]

let location = function Valid { location; _ } -> location | No_location { location; _ } -> location

let make ~id ~location ~sort_order = { id; location; sort_order }

(* sort items with sort_order in [t] *)
let sort_items t =
  let is_not_directory = not % File_item.is_directory in
  match t with
  | Valid ({ items; sort_order; _ } as t) ->
      let sort_fun = File_item.compare_by sort_order in
      let dirs = List.filter File_item.is_directory items and files = List.filter is_not_directory items in
      Valid { t with items = List.concat [ List.sort sort_fun dirs; List.sort sort_fun files ] }
  | No_location _ as v                    -> v

let reload result = function
  | Valid t       -> (
      match result with
      | `No_location   -> No_location { id = t.id; location = t.location; sort_order = t.sort_order }
      | `Scanned items -> Valid { t with items } |> sort_items )
  | No_location t -> (
      match result with
      | `No_location   -> No_location t
      | `Scanned items -> Valid { id = t.id; location = t.location; sort_order = t.sort_order; items } |> sort_items )

let scan result t =
  match result with
  | `No_location   -> No_location { id = t.id; location = t.location; sort_order = t.sort_order }
  | `Scanned items -> Valid { id = t.id; location = t.location; sort_order = t.sort_order; items } |> sort_items

let change_location ~location = function
  | Valid { id; sort_order; _ }       -> { id; sort_order; location }
  | No_location { id; sort_order; _ } -> { id; sort_order; location }

(* make {!Item_map} from [items] *)
let make_item_map items =
  List.fold_left (fun map item -> Item_map.add (File_item.id item) item map) Item_map.empty items

(* generic function to update items of the [t] with [f] *)
let change_items_from_ids ~f ~ids = function
  | Valid t            ->
      let item_map = make_item_map t.items in
      let marked_items =
        List.fold_left
          (fun map id -> match Item_map.find_opt id map with None -> map | Some item -> Item_map.add id (f item) map)
          item_map ids
      in
      Valid { t with items = Item_map.to_seq marked_items |> Seq.map snd |> List.of_seq } |> sort_items
  | No_location _ as v -> v

let mark_items = change_items_from_ids ~f:File_item.mark

let unmark_items = change_items_from_ids ~f:File_item.unmark

let items = function Valid { items; _ } -> items | No_location _ -> []

let marked_items t = items t |> List.filter (function File_item.Marked _ -> true | Unmarked _ -> false)

let find_item ~id t = items t |> List.find_opt (fun item -> File_item.id item |> File_item.Id.equal id)
