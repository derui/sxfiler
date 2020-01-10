open Sxfiler_core
module D = Sxfiler_domain.Filer
module L = Sxfiler_domain.Location_history
module G = Sxfiler_server_generated

let of_domain (t : D.t) =
  {
    G.Filer.Filer.id = Uuidm.to_string t.D.id;
    name = t.name;
    fileList = File_list.of_domain t.file_list |> Option.some;
    sortOrder = Types.Sort_type.of_domain t.sort_order;
    markedItems = D.Marked_item_set.to_seq t.marked_items |> List.of_seq;
    history = Location_history.of_domain t.history |> Option.some;
  }

let to_domain (t : G.Filer.Filer.t) =
  let id = Uuidm.of_string t.id |> Option.get_exn in
  let history =
    Option.(t.history >|= Location_history.to_domain) |> Option.get ~default:(fun () -> L.make ())
  in
  D.make ~id ~name:t.name
    ~file_list:(Option.get_exn t.fileList |> File_list.to_domain)
    ~sort_order:(Types.Sort_type.to_domain t.sortOrder)
    ~marked_items:(D.Marked_item_set.of_list t.markedItems)
    ~history
