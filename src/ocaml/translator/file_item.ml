open Sxfiler_core
module D = Sxfiler_domain
module FI = D.File_item
module G = Sxfiler_generated

type error =
  | No_stat      of string
  | Invalid_stat of File_stat.error
  | Invalid_path of string
[@@deriving eq, show]

type t = G.Filer.FileItem.t

let of_domain (t : FI.t) =
  let item = FI.item t in
  let stat = File_stat.of_domain item.stat |> Option.some in
  {
    G.Filer.FileItem.id = FI.id t |> FI.Id.value;
    name = Path.basename item.full_path;
    full_path = Path.to_string item.full_path;
    stat;
    parent = Path.dirname item.full_path;
    has_link_path = FI.is_symlink t;
    link_path = (match item.stat.kind with D.File_stat.Kind.Symlink v -> Path.to_string v | _ -> "");
    marked = (match t with FI.Marked _ -> true | FI.Unmarked _ -> false);
  }

let to_domain (t : t) =
  let open Result.Infix in
  let* stat = Option.to_result ~none:(No_stat t.id) t.stat in
  let* stat = File_stat.to_domain stat |> Result.map_error (fun v -> Invalid_stat v) in
  let* full_path =
    Path.of_string t.full_path |> Result.map_error (function Path.Empty_path -> Invalid_path t.full_path)
  in
  let item = FI.make ~id:(FI.Id.make t.id) ~full_path ~stat in
  let item = if t.marked then FI.mark item else FI.unmark item in
  Ok item
