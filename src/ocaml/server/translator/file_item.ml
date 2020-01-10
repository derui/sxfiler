open Sxfiler_core
module D = Sxfiler_domain.File_item
module G = Sxfiler_server_generated

type t = G.Filer.FileItem.t

let of_domain (t : D.t) =
  {
    G.Filer.FileItem.id = t.D.id;
    name = Path.basename t.D.full_path;
    fullPath = Path.to_string t.D.full_path;
    stat = File_stat.of_domain t.D.stat |> Option.some;
    parent = Path.dirname t.D.full_path;
    hasLinkPath = Option.is_some t.link_path;
    linkPath =
      Option.(t.link_path >|= fun v -> Path.to_string v) |> Option.get ~default:(fun () -> "");
  }

let to_domain (t : t) =
  {
    D.id = t.id;
    full_path = Path.of_string t.fullPath;
    stat = Option.get_exn t.stat |> File_stat.to_domain;
    link_path =
      (match t.hasLinkPath with true -> Path.of_string t.linkPath |> Option.some | false -> None);
  }
