type id = File_item.Id.t [@@deriving eq, show]

type sort_level = Types.sort_level [@@deriving eq, show]

type sort_type = Types.Sort_type.t

module Order_mapping = struct
  module Order_pair = Map.Make (struct
    type t = id

    let compare = File_item.Id.compare
  end)

  type t = { order_pairs : sort_level Order_pair.t }

  let show t =
    let str = Buffer.create 0 in
    Order_pair.iter
      (fun id v -> Buffer.add_string str @@ Printf.sprintf "%s: %d" (File_item.Id.value id) v)
      t.order_pairs;
    Buffer.to_seq str |> String.of_seq

  let make order_pairs =
    let order_pairs =
      List.fold_left (fun pairs (id, sort_level) -> Order_pair.add id sort_level pairs) Order_pair.empty order_pairs
    in
    { order_pairs }

  let equal v1 v2 = Order_pair.equal Int.equal v1.order_pairs v2.order_pairs
end

type t = {
  sort_type : sort_type;
  order_mapping : Order_mapping.t;
}

let show t =
  Printf.sprintf "{sort_type: %s, order_mapping: %s}" (Types.Sort_type.show t.sort_type)
    (Order_mapping.show t.order_mapping)

let pp fmt t = Format.fprintf fmt "%s" @@ show t

let equal v1 v2 =
  Types.Sort_type.equal v1.sort_type v2.sort_type && Order_mapping.equal v1.order_mapping v2.order_mapping

let make ~sort_type = { sort_type; order_mapping = Order_mapping.make [] }

let update_order_mapping ?sort_type ~file_items { sort_type = sort_type'; _ } =
  let sort_type = Option.value sort_type ~default:sort_type' in
  let open Sxfiler_core in
  let is_not_directory = not % File_item.is_directory in
  let sort_fun = File_item.compare_by sort_type in
  let dirs = List.filter File_item.is_directory file_items and files = List.filter is_not_directory file_items in
  let sorted_items = List.concat [ List.sort sort_fun dirs; List.sort sort_fun files ] in
  {
    sort_type;
    order_mapping = sorted_items |> List.mapi (fun index v -> (File_item.id v, index)) |> Order_mapping.make;
  }

let to_alist { order_mapping; _ } = Order_mapping.Order_pair.to_seq order_mapping.order_pairs |> List.of_seq
