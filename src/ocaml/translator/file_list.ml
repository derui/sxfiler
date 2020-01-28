open Sxfiler_core
module D = Sxfiler_domain.File_list
module G = Sxfiler_generated

let of_domain (t : D.scanned) =
  let id = match t with D.Valid { id; _ } -> id | No_location { id; _ } -> id in
  let location = match t with D.Valid { location; _ } -> location | No_location { location; _ } -> location in
  let sort_order =
    match t with D.Valid { sort_order; _ } -> sort_order | D.No_location { sort_order; _ } -> sort_order
  in
  {
    G.Filer.FileList.id = D.Id.value id;
    location = Path.to_string location;
    items = D.items t |> List.map File_item.of_domain;
    sort_order = Types.Sort_type.of_domain sort_order;
  }
