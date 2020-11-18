open Sxfiler_core
module D = Sxfiler_domain
module G = Sxfiler_generated

let of_domain (t : D.File_list.scanned) =
  let id = match t with D.File_list.Valid { id; _ } -> id | No_location { id; _ } -> id in
  let location =
    match t with D.File_list.Valid { location; _ } -> location | No_location { location; _ } -> location
  in
  let file_item_order =
    match t with
    | D.File_list.Valid { file_item_order; _ } -> file_item_order
    | D.File_list.No_location { file_item_order; _ } -> file_item_order
  in
  let file_item_order =
    D.File_item_order.to_alist file_item_order
    |> List.map (fun (id, sort_level) -> { G.Filer.FileItemOrder.file_id = D.File_item.Id.value id; sort_level })
  in
  {
    G.Filer.FileList.id = D.File_list.Id.value id;
    location = Path.to_string location;
    items = D.File_list.items t |> List.map File_item.of_domain;
    file_item_orders = file_item_order;
  }
