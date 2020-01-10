open Sxfiler_core
module D = Sxfiler_domain.File_list
module G = Sxfiler_server_generated

type t = G.Filer.FileList.t

let of_domain (t : D.t) =
  {
    G.Filer.FileList.location = Path.to_string t.location;
    items = List.map File_item.of_domain t.items;
  }

let to_domain (t : t) =
  { D.location = Path.of_string t.location; items = List.map File_item.to_domain t.items }
